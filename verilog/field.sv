module srdlField #(
   parameter WIDTH        = 1,
   parameter RCLR         = 0,
   parameter RSET         = 1,
   parameter SW           = "rw",
   parameter HW           = "rw",
   parameter COUNTER      = 0,
   parameter INCRWIDTH    = 1,
   parameter DECRWIDTH    = 1,
) (
   input [WIDTH - 1:0] d,
   input [WIDTH - 1:0] sw_wdata,
   input [WIDTH - 1:0] hw_wdata,
   input               hw_we,
   input               rd,
   input               wr,
   input               acc,

   //Counter props
   input               incr,
   input               decr,
   input [INCRWIDTH - 1:0] incrvalue,
   input [DECRWIDTH - 1:0] decrvalue,
   output              overflow,
   output              underflow,

   input   [WIDTH-1:0] incrsaturate_lhs,
   input   [WIDTH-1:0] incrthreshold_lhs,
   input   [WIDTH-1:0] decrsaturate_lhs,
   input   [WIDTH-1:0] decrthreshold_lhs,

   output   [WIDTH-1:0] rhscrsaturate_rhs,
   output   [WIDTH-1:0] rhscrthreshold_rhs,
   output   [WIDTH-1:0] decrsaturate_rhs,
   output   [WIDTH-1:0] decrthreshold_rhs,

   output [WIDTH - 1:0] q,
);

always_comb begin
   q = d;

   if (RCLR && rd && acc)
      q = '0;

   if (RSET && rd && acc)
      q = '1;

   if (wr && (SW == "rw" || SW == "wr" || SW == "W")) begin
      if (WOSET)
         q = q |  wdata;
      else if (WOCLR)
         q = q & ~wdata;
      else
         q = wdata;
   end else if (SINGLEPULSE) begin
      q = 0;
   end

   if (HW == "rw" || HW == "wr" || HW == "W") begin
      if (WE && hw_we)
         q = hw_wdata;
      else if (WEL && !hw_we)
         q = hw_wdata;
      else
         q = hw_wdata;
   end

   underflow = 0
   overflow = 0;
   if (COUNTER) begin
      case (decr, incr)
         2'b01 : begin
            {wrap, q} = q + incrvalue;
            if (wrap || q > incrsaturate_lhs)
               q = incrsaturate_lhs;
            overflow = wrap;
         end
         2'b10 : begin
            {wrap, q} = q - decrvalue;
            if (wrap || q < decrsaturate_lhs)
               q = decrsaturate_lhs;
            underflow = wrap;
         end
         2'b11 : {wrap, q} = q + incrvalue - decrvalue;
            {wrap, q} = q + incrvalue - decrvalue;
            if (wrap) begin
               underflow = decrvalue > incrvalue;
               overflow  = decrvalue < incrvalue;
            end
            if (underflow || q < decrsaturate_lhs)
               q = decrsaturate_lhs;
            if (overflow  || q > incrsaturate_lhs)
               q = incrsaturate_lhs;
         end
         default : begin end
      endcase
      incrthreshold_rhs = q = incrthreshold_lhs;
      decrthreshold_rhs = q = decrthreshold_lhs;
      incrsaturate_rhs  = q = incrsaturate_lhs;
      decrsaturate_rhs  = q = decrsaturate_lhs;
   end

   if (hwclr)
      q = 'b0;
end

