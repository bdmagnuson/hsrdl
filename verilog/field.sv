module srdlField #(
   parameter RESET        = 0,
   parameter WIDTH        = 1,
   parameter RCLR         = 0,
   parameter RSET         = 0,
   parameter SW           = "rw",
   parameter HW           = "r",
   parameter COUNTER      = 0,
   parameter INCRWIDTH    = 1,
   parameter DECRWIDTH    = 1,
   parameter NEXT         = 0,
   parameter INTR         = "NONE"
) (
   input wire               clk,
   input wire               rst_l,

   input wire [WIDTH - 1:0] next_in,
   input wire [WIDTH - 1:0] sw_wdata,
   input wire [WIDTH - 1:0] hw_wdata,
   input wire               hw_we,
   input wire               rd,
   input wire               wr,
   input wire               acc,
   input wire               hwset,
   input wire               hwclr,

   output reg               intr,
   input wire               reg_intr,

   //Counter props
   input wire               incr,
   input wire               decr,
   input wire [INCRWIDTH - 1:0] incrvalue,
   input wire [DECRWIDTH - 1:0] decrvalue,
   output reg              overflow,
   output reg              underflow,

   input wire   [WIDTH-1:0] incrsaturate_lhs,
   input wire   [WIDTH-1:0] incrthreshold_lhs,
   input wire   [WIDTH-1:0] decrsaturate_lhs,
   input wire   [WIDTH-1:0] decrthreshold_lhs,

   output reg   [WIDTH-1:0] rhscrsaturate_rhs,
   output reg   [WIDTH-1:0] rhscrthreshold_rhs,
   output reg   [WIDTH-1:0] decrsaturate_rhs,
   output reg   [WIDTH-1:0] decrthreshold_rhs,

   output reg [WIDTH - 1:0] q,
);

reg [WIDTH - 1:0] next;
reg [WIDTH - 1:0] sticky_mask, r_sticky_mask;
reg [WIDTH - 1:0] hw_wdata_mask;
reg               r_intr;

always @(posedge clk)
   if (!rst_l) begin
      q <= RESET;
      r_sticky_mask <= '0;
      r_intr <= 'b0;
   end else begin
      q <= next;
      r_sticky_mask <= sticky_mask;
      if (!NONSTICKY)
         r_intr <= intr;
   end
end

always_comb begin
   incrthreshold_rhs = q == incrthreshold_lhs;
   decrthreshold_rhs = q == decrthreshold_lhs;
   incrsaturate_rhs  = q == incrsaturate_lhs;
   decrsaturate_rhs  = q == decrsaturate_lhs;
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
         next = next |  wdata;
      end else if (WOCLR)
         next = next & ~wdata;
         intr = intr & (next != 0);
         sticky_mask = sticky_mask & ~wdata;
      else begin
         next = wdata;
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

   underflow = 0
   overflow = 0;
   if (COUNTER) begin
      case (decr, incr)
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
         2'b11 : {wrap, next} = next + incrvalue - decrvalue;
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
      "POSEDGE"  : intr = |next && ~|d
      "NEGEDGE"  : intr = ~|next && |d;
      "BOTHEDGE" : intr = next != d;
      "NONE"     : intr = 0;
      default    : intr = 0;
   endcase
end

