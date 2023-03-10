--
--  Copyright (C) 2018-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Notes about names of variables: its case sensitivity is platform dependent.
--  On case insensitive platforms original casing is returned by Keys function.

with Spawn.String_Vectors;

private with Ada.Containers.Indefinite_Ordered_Maps;

package Spawn.Environments is

   type Process_Environment is tagged private;

   procedure Clear (Self : in out Process_Environment'Class);
   --  Reset given Process_Environment to an empty state.

   function Contains
    (Self : Process_Environment'Class; Name : UTF_8_String) return Boolean;
   --  Check if given Process_Environment has variable with given Name.

   procedure Insert
    (Self  : in out Process_Environment'Class;
     Name  : UTF_8_String;
     Value : UTF_8_String);
   --  Insert variable into Process_Environment, replace existing variable
   --  with the same name if any.

   procedure Insert
    (Self        : in out Process_Environment'Class;
     Environemnt : Process_Environment'Class);
   --  Insert all variables into Process_Environment.

   function Keys (Self : Process_Environment'Class)
     return Spawn.String_Vectors.UTF_8_String_Vector;
   --  Return list of names of variables.

   procedure Remove
    (Self : in out Process_Environment'Class;
     Name : UTF_8_String);
   --  Delete variable from Process_Environment. Do nothing if no such variable

   function Value
    (Self    : Process_Environment'Class;
     Name    : UTF_8_String;
     Default : UTF_8_String := "") return UTF_8_String;
   --  Get value of an variable from Process_Environment

   function Search_Path
    (Self : Process_Environment'Class;
     File : UTF_8_String;
     Name : UTF_8_String := "PATH") return UTF_8_String;
   --  Take an environment variable with given Name as directory list, then
   --  search for the File in each of this directory. Return path to found file
   --  or empty string if not found.

   function System_Environment return Process_Environment;

private

   function Less (Left, Right : UTF_8_String) return Boolean;
   function Equal (Left, Right : UTF_8_String) return Boolean;

   package UTF_8_String_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type     => UTF_8_String,
      Element_Type => UTF_8_String,
      "<"          => Less,
      "="          => Equal);  --  Spawn.Internal.Environments."=");

   type Process_Environment is tagged record
      Map : UTF_8_String_Maps.Map;
   end record;

end Spawn.Environments;
