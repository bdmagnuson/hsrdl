# hsrdl

hsrdl is a parser/generator for SRDL 1.0 register definition files.  The specification is available [here](http://www.accellera.org/images/downloads/standards/systemrdl/SystemRDL_2.0_Jan2018.pdf).

The are two currently supported backends that emit Verilog implemeting the register definition and a UVM RAL package for integration into a testbench.

# Example #
```
addrmap counter_overflow {

   reg {
      field {} boring[2]; //Two bits wide
      field {} writeme;
   } ordinary_reg; //Anonymous reg

   field some_counter {
      counter;
      we;
   }; //Field defined to be used later

   reg some_counter_reg {
      some_counter count[16];
   };

   some_counter_reg count1_low;
   some_counter_reg count1_high;

   count1_high.count->incr = count1_low.count->overflow;

};
```

The above defines three registers.  The first is an ordinary read/write register with a couple of fields.  The other two are counters where the increment of the 2nd is tied to the overflow event of the 1st.  The 1st counters increment will be a primary input of the register block.

# TODO #
* Properties that support more than one type on RHS
* More property assignment validation
  * Post assigns to non dynamic properties
* Catch field instances outside a Reg
* Restore external interfaces
* Signal components

## Currently unsupported properties ##
*    async
*    activelow
*    activehigh
*    cpuif_reset
*    encode
*    errextbus
*    hwenable
*    hwmask
*    dontcompare
*    donttest
*    field_reset
*    precedence
*    resetsignal
*    signalwidth
*    swwe
*    swwel

## No plans (for now) to support ## 
*   bigendian
*   littleendian
*   bridge
*   shared
*   rsvdset
*   rsvdsetX
*   lsb0
*   msb0


