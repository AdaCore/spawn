with Ada.Text_IO;

package body Signals is

   --------------------
   -- Signal_Handler --
   --------------------

   protected body Signal_Handler is

      --------------------
      -- On_Term_Signal --
      --------------------

      procedure On_Term_Signal is
      begin
         Ada.Text_IO.Put_Line ("Got TERM");
      end On_Term_Signal;

   end Signal_Handler;

end Signals;
