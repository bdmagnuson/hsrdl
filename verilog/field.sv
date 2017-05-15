module srdlField #(
   parameter RESET        = 0,
   parameter WIDTH        = 1,
   parameter RCLR         = 0,
   parameter RSET         = 1,
   parameter SW           = "rw",
   parameter HW           = "rw",
   parameter COUNTER      = 0,
   parameter INCRWIDTH    = 1,
   parameter DECRWIDTH    = 1,
   parameter NEXT         = 0
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

always @(posedge clk)
   if (!rst_l)
      q <= RESET;
   else
      q <= next;

always_comb begin
   incrthreshold_rhs = q == incrthreshold_lhs;
   decrthreshold_rhs = q == decrthreshold_lhs;
   incrsaturate_rhs  = q == incrsaturate_lhs;
   decrsaturate_rhs  = q == decrsaturate_lhs;
end

always_comb begin
   next = q;

   if (RCLR && rd && acc)
      next = '0;

   if (RSET && rd && acc)
      next = '1;

   if (wr && (SW == "rw" || SW == "wr" || SW == "W")) begin
      if (WOSET)
         next = next |  wdata;
      else if (WOCLR)
         next = next & ~wdata;
      else
         next = wdata;
   end else if (SINGLEPULSE) begin
      next = 0;
   end

   if (HW == "rw" || HW == "wr" || HW == "W") begin
      if (WE && hw_we)
         next = hw_wdata;
      else if (WEL && !hw_we)
         next = hw_wdata;
      else
         next = hw_wdata;
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
      next = 'b0;

   if (NEXT)
      next = next_in;
end

