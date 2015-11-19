# hsrdl
Haskell based SRDL parser/generator

This module will parse SystemRDL (SRDL) into an AST which can then be handed
off to various backend to generate synthesizable RTL, UVM register classes,
documentation, C headers, etc... 

The standard is avialable at http://accellera.org/images/downloads/standards/SystemRDL_1.0.zip

# Example SRDL
```
addrmap top_map {
   reg myReg {
      //Fields are defined in LSB order by default
      field {
         hw = r;
      } first;
      field {
         we;               //Properties with no RHS are implicity parsed as '= true'
      } second[4];         //Specified width
      field {} width[9:3]; //Specified bit positions
   } first_reg[4];         //Can specifiy arrays of components

   myReg another_one;      //Once defined can be instantiated again

   first_reg[2].second->reset = 3; //Properties can be overridden

   // There are many predefined properites but user can specify addition ones
   property some_prop {
      type = boolean;
      component = field;
      default = false;
   };

   reg { //Components can be anonymous
      field {
         some_prop = true;
      } f1;
   } r2;
};
```



