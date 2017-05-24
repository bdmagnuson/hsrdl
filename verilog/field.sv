module srdlField #(
   parameter RESET        = 0,
   parameter WIDTH        = 1,
   parameter WOCLR        = 0,
   parameter WOSET        = 0,
   parameter RCLR         = 0,
   parameter RSET         = 0,
   parameter SW           = "rw",
   parameter HW           = "r",
   parameter COUNTER      = 0,
   parameter INCRWIDTH    = 1,
   parameter DECRWIDTH    = 1,
   parameter NEXT         = 0,
   parameter INTR         = "NONE",
   parameter STICKY       = 1,
   parameter STICKYBIT    = 0,
   parameter WE           = 0,
   parameter WEL          = 0,
   parameter SINGLEPULSE  = 0
) (
   input wire                 clk,
   input wire                 rst_l,

   input wire     [WIDTH-1:0] next_in,
   input wire     [WIDTH-1:0] sw_wdata,
   input wire     [WIDTH-1:0] hw_wdata,
   input wire                 hw_we,
   input wire                 rd,
   input wire                 wr,
   input wire                 acc,
   input wire                 hwset,
   input wire                 hwclr,

   output reg                 intr,
   input wire                 reg_intr,

   //Counter props
   input wire                 incr,
   input wire                 decr,
   input wire [INCRWIDTH-1:0] incrvalue,
   input wire [DECRWIDTH-1:0] decrvalue,
   output reg                 overflow,
   output reg                 underflow,

   input wire     [WIDTH-1:0] incrsaturate_lhs,
   input wire     [WIDTH-1:0] decrsaturate_lhs,
   input wire     [WIDTH-1:0] incrthreshold_lhs,
   input wire     [WIDTH-1:0] decrthreshold_lhs,

   output reg     [WIDTH-1:0] incrsaturate_rhs,
   output reg     [WIDTH-1:0] decrsaturate_rhs,
   output reg     [WIDTH-1:0] incrthreshold_rhs,
   output reg     [WIDTH-1:0] decrthreshold_rhs,

   output reg                 anded,
   output reg                 ored,
   output reg                 xored,

   output reg     [WIDTH-1:0] q
);

reg [WIDTH - 1:0] next;
reg [WIDTH - 1:0] sticky_mask, r_sticky_mask;
reg [WIDTH - 1:0] hw_wdata_mask;
reg               r_intr;
reg               wrap;

always @(posedge clk) begin
   if (!rst_l) begin
      q <= RESET;
      r_sticky_mask <= '0;
      r_intr <= 'b0;
   end else begin
      q <= next;
      r_sticky_mask <= sticky_mask;
      if (STICKY)
         r_intr <= intr;
   end
end

always_comb begin
   incrthreshold_rhs = q == incrthreshold_lhs;
   decrthreshold_rhs = q == decrthreshold_lhs;
   incrsaturate_rhs  = q == incrsaturate_lhs;
   decrsaturate_rhs  = q == decrsaturate_lhs;

   ored = |q;
   anded = &q;
   xored = ^q;
end

always_comb begin
   next        = q;
   sticky_mask = r_sticky_mask || {WIDTH{STICKY && reg_intr}};
   intr        = r_intr;

   if (RCLR && rd && acc) begin
      next = '0;
      sticky_mask = '0;
      intr = '0;
   end

   if (RSET && rd && acc)
      next = '1;

   if (wr && (SW == "rw" || SW == "wr" || SW == "W")) begin
      if (WOSET) begin
         next = next | sw_wdata;
      end else if (WOCLR) begin
         next = next & ~sw_wdata;
         intr = intr & (next != 0);
         sticky_mask = sticky_mask & ~sw_wdata;
      end else begin
         next = sw_wdata;
         intr = 0;
         sticky_mask = '0;
      end
   end else if (SINGLEPULSE) begin
      next = 0;
   end

   if (HW == "rw" || HW == "wr" || HW == "W") begin
      hw_wdata_mask = (next & r_sticky_mask) | (hw_wdata & ~r_sticky_mask);
      if (WE && hw_we)
         next = hw_wdata_mask;
      else if (WEL && !hw_we)
         next = hw_wdata_mask;
      else
         next = hw_wdata_mask;
   end

   underflow = 0;
   overflow = 0;
   if (COUNTER) begin
      case ({decr, incr})
         2'b01 : begin
            {wrap, next} = next + incrvalue;
            if (wrap || next > incrsaturate_lhs)
               next = incrsaturate_lhs;
            overflow = wrap;
         end
         2'b10 : begin
            {wrap, next} = next - decrvalue;
            if (wrap || next < decrsaturate_lhs)
               next = decrsaturate_lhs;
            underflow = wrap;
         end
         2'b11 : begin
            {wrap, next} = next + incrvalue - decrvalue;
            if (wrap) begin
               underflow = decrvalue > incrvalue;
               overflow  = decrvalue < incrvalue;
            end
            if (underflow || next < decrsaturate_lhs)
               next = decrsaturate_lhs;
            if (overflow  || next > incrsaturate_lhs)
               next = incrsaturate_lhs;
         end
         default : begin end
      endcase
   end

   if (hwclr)
      next = '0;

   if (hwset)
      next = '1;

   if (NEXT)
      next = next_in;

   if (STICKYBIT)
      sticky_mask |= next;

   case (INTR)
      "LEVEL"    : intr = |next;
      "POSEDGE"  : intr = |next && ~|q;
      "NEGEDGE"  : intr = ~|next && |q;
      "BOTHEDGE" : intr = next != q;
      "NONE"     : intr = 0;
      default    : intr = 0;
   endcase
end

endmodule

