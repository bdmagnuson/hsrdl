module TestStrings (
     textb
   , texta
   ) where

textb="\
\addrmap coarse_delay {\n\
\   reg {\n\
\      field { } delay[10];\n\
\   } delay[2];\n\
\   delay->arr_prop = 4;\n\
\};"


texta="\
\addrmap coarse_delay {\n\
\   reg {\n\
\      name = \"l-value delay\";\n\
\      desc = \"\";\n\
\      field { } delay[10];\n\
\   } delay[16];\n\
\\n\
\   delay[0].delay->desc = \"reserved\";\n\
\   delay[1].delay->desc = \"delay for 0->1 transition\";\n\
\   delay[2].delay->desc = \"delay for 0->2 transition\";\n\
\   delay[3].delay->desc = \"delay for 0->3 transition\";\n\
\   delay[4].delay->desc = \"delay for 1->0 transition\";\n\
\   delay[5].delay->desc = \"reserved\";\n\
\   delay[6].delay->desc = \"delay for 1->2 transition\";\n\
\   delay[7].delay->desc = \"delay for 1->3 transition\";\n\
\   delay[8].delay->desc = \"delay for 2->0 transition\";\n\
\   delay[9].delay->desc = \"delay for 2->1 transition\";\n\
\   delay[10].delay->desc = \"reserved\";\n\
\   delay[11].delay->desc = \"delay for 2->3 transition\";\n\
\   delay[12].delay->desc = \"delay for 3->0 transition\";\n\
\   delay[13].delay->desc = \"delay for 3->1 transition\";\n\
\   delay[14].delay->desc = \"delay for 3->2 transition\";\n\
\   delay[15].delay->desc = \"reserved\";\n\
\\n\
\   reg foo {};\n\
\   foo blah;\n\
\   reg {\n\
\      desc = \"maps outgoing 2b level to a 4b value\";\n\
\      field {\n\
\         desc = \"level 0 encoding\";\n\
\      } lvl0[4];\n\
\      field {\n\
\         desc = \"level 1 encoding\";\n\
\      } lvl1[4];\n\
\      field {\n\
\         desc = \"level 2 encoding\";\n\
\      } lvl2[4];\n\
\      field {\n\
\         desc = \"level 3 encoding\";\n\
\      } lvl3[4];\n\
\   } lvl_out_xlate;\n\
\\n\
\   reg {\n\
\      desc = \"maps incoming 3b level to a 2b value\";\n\
\      field { desc = \"level 0 encoding\"; } lvl0[2];\n\
\      field { desc = \"level 1 encoding\"; } lvl1[2];\n\
\      field { desc = \"level 2 encoding\"; } lvl2[2];\n\
\      field { desc = \"level 3 encoding\"; } lvl3[2];\n\
\      field { desc = \"level 4 encoding\"; } lvl4[2];\n\
\      field { desc = \"level 5 encoding\"; } lvl5[2];\n\
\      field { desc = \"level 6 encoding\"; we;} lvl6[2];\n\
\   } lvl_in_xlate;\n\
\};"

