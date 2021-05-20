with Ada.Interrupts.Names;

package Signals is

   pragma Unreserve_All_Interrupts;

   protected Signal_Handler is

      procedure On_Term_Signal;

      pragma Attach_Handler (On_Term_Signal, Ada.Interrupts.Names.SIGTERM);

   end Signal_Handler;
end Signals;
