--  SPDX-License-Identifier: MIT

with Ada.Characters.Wide_Wide_Latin_1;

with Ada_Pretty;

with League.String_Vectors;

with Compiler.Context;
with Compiler.Field_Descriptors;

package body Compiler.File_Descriptors.JSON is

   F : Ada_Pretty.Factory renames Compiler.Context.Factory;

   use type Ada_Pretty.Node_Access;

   function "+"
     (Text : Wide_Wide_String) return League.Strings.Universal_String
   renames League.Strings.To_Universal_String;

   function Generate_Field
     (Pkg   : League.Strings.Universal_String;
      Field : Google.Protobuf.Descriptor.Field_Descriptor_Proto;
      Acc   : League.Strings.Universal_String) return Ada_Pretty.Node_Access;

   function Generate_Field
     (Pkg   : League.Strings.Universal_String;
      Field : Google.Protobuf.Descriptor.Field_Descriptor_Proto;
      Acc   : League.Strings.Universal_String) return Ada_Pretty.Node_Access
   is

      --  Generate_*_Statement: helper functions to generate Stream.Write_*
      --  statements for reuse.

      function Generate_Stream_Write_Statement
        (Stream_Method : League.Strings.Universal_String;
         Argument      : Ada_Pretty.Node_Access) return Ada_Pretty.Node_Access
      with Inline;
      function Generate_Bool_Statement
        (Acc : League.Strings.Universal_String) return Ada_Pretty.Node_Access
      with Inline;
      function Generate_String_Statement
        (Acc : League.Strings.Universal_String) return Ada_Pretty.Node_Access
      with Inline;
      function Generate_Bytes_Statement
        (Acc : League.Strings.Universal_String) return Ada_Pretty.Node_Access
      with Inline;
      function Generate_Null_Statement return Ada_Pretty.Node_Access
      with Inline;
      function Generate_Integer_Statement
        (Acc : League.Strings.Universal_String) return Ada_Pretty.Node_Access
      with Inline;
      function Generate_Int64_Statement
        (Acc : League.Strings.Universal_String) return Ada_Pretty.Node_Access
      with Inline;
      function Generate_Float_Statement
        (Acc : League.Strings.Universal_String; Needs_Conv : Boolean)
         return Ada_Pretty.Node_Access
      with Inline;
      function Generate_Enum_Statement
        (Acc : League.Strings.Universal_String; Is_Null_Value : Boolean)
         return Ada_Pretty.Node_Access
      with Inline;

      function Generate_Stream_Write_Statement
        (Stream_Method : League.Strings.Universal_String;
         Argument      : Ada_Pretty.Node_Access) return Ada_Pretty.Node_Access
      is
      begin
         return
           F.New_Statement
             (F.New_Apply (F.New_Selected_Name (Stream_Method), Argument));
      end Generate_Stream_Write_Statement;

      function Generate_Bool_Statement
        (Acc : League.Strings.Universal_String) return Ada_Pretty.Node_Access
      is
      begin
         return
           Generate_Stream_Write_Statement
             (+"Stream.Write_Boolean", F.New_Selected_Name (Acc));
      end Generate_Bool_Statement;

      function Generate_String_Statement
        (Acc : League.Strings.Universal_String) return Ada_Pretty.Node_Access
      is
         use type League.Strings.Universal_String;
      begin
         return
           Generate_Stream_Write_Statement
             (+"Stream.Write_String", F.New_Name (+"+" & Acc));
      end Generate_String_Statement;

      function Generate_Bytes_Statement
        (Acc : League.Strings.Universal_String) return Ada_Pretty.Node_Access
      is
      begin
         return
           Generate_Stream_Write_Statement
             (+"Stream.Write_Bytes", F.New_Selected_Name (Acc));
      end Generate_Bytes_Statement;

      function Generate_Null_Statement return Ada_Pretty.Node_Access is
      begin
         return F.New_Statement (F.New_Selected_Name (+"Stream.Write_Null"));
      end Generate_Null_Statement;

      function Generate_Integer_Statement
        (Acc : League.Strings.Universal_String) return Ada_Pretty.Node_Access
      is
      begin
         return
           Generate_Stream_Write_Statement
             (+"Stream.Write_Integer",
              F.New_Apply
                (F.New_Name (+"Long_Long_Integer"),
                 F.New_Selected_Name (Acc)));
      end Generate_Integer_Statement;

      function Generate_Int64_Statement
        (Acc : League.Strings.Universal_String) return Ada_Pretty.Node_Access
      is
      begin
         return
           Generate_Stream_Write_Statement
             (+"Stream.Write_Integer", F.New_Selected_Name (Acc));
      end Generate_Int64_Statement;

      function Generate_Float_Statement
        (Acc : League.Strings.Universal_String; Needs_Conv : Boolean)
         return Ada_Pretty.Node_Access is
      begin
         if Needs_Conv then
            return
              F.New_Block
                (Statements   =>
                   F.New_Statement
                     (F.New_Apply
                        (F.New_Selected_Name (+"Stream.Write_Float"),
                         F.New_Apply
                           (F.New_Name (+"Interfaces.IEEE_Float_64"),
                            F.New_Selected_Name (Acc)))),
                 Declarations =>
                   F.New_Pragma
                     (F.New_Name (+"Suppress"), F.New_Name (+"Range_Check")));
         else
            return
              F.New_Block
                (Statements   =>
                   F.New_Statement
                     (F.New_Apply
                        (F.New_Selected_Name (+"Stream.Write_Float"),
                         F.New_Selected_Name (Acc))),
                 Declarations =>
                   F.New_Pragma
                     (F.New_Name (+"Suppress"), F.New_Name (+"Range_Check")));
         end if;
      end Generate_Float_Statement;

      function Generate_Enum_Statement
        (Acc : League.Strings.Universal_String; Is_Null_Value : Boolean)
         return Ada_Pretty.Node_Access
      is
         use type League.Strings.Universal_String;
      begin
         if Is_Null_Value then
            return Generate_Null_Statement;
         else
            if Compiler.Context.Always_Print_Enums_As_Ints then
               return
                 Generate_Stream_Write_Statement
                   (+"Stream.Write_Integer",
                    F.New_Apply
                      (F.New_Name (+"Long_Long_Integer"),
                       F.New_Selected_Name (Acc & "'Enum_Rep")));
            end if;
            return
              Generate_Stream_Write_Statement
                (+"Stream.Write_String", F.New_Selected_Name (Acc & "'Image"));
         end if;
      end Generate_Enum_Statement;

      use type League.Strings.Universal_String;
      use all type Google.Protobuf.Descriptor.PB_Type;
      Is_Enum    : constant Boolean :=
        Compiler.Field_Descriptors.Is_Enum (Field);
      Is_Message : constant Boolean :=
        Compiler.Field_Descriptors.Is_Message (Field);
   begin
      if Is_Message then
         declare
            Target : constant Compiler.Context.Named_Type :=
              Compiler.Context.Named_Types (Field.Type_Name.Value);
         begin
            if Target.Ada_Type.Package_Name = Pkg then
               return
                 F.New_Statement
                   (F.New_Apply
                      (F.New_Name (+"Write"),
                       F.New_List
                         (F.New_Name (+"Stream"), F.New_Selected_Name (Acc))));
            else
               return
                 F.New_Statement
                   (F.New_Apply
                      (F.New_Selected_Name
                         (Target.Ada_Type.Package_Name & ".JSON.Write"),
                       F.New_List
                         (F.New_Name (+"Stream"), F.New_Selected_Name (Acc))));
            end if;
         end;
      elsif Is_Enum then
         declare
            Is_Null_Value : constant Boolean :=
              Field.Type_Name.Is_Set
              and then Field.Type_Name.Value = +".google.protobuf.NullValue";
         begin
            return Generate_Enum_Statement (Acc, Is_Null_Value);
         end;
      elsif Field.PB_Type.Is_Set then
         case Field.PB_Type.Value is
            when TYPE_BOOL                =>
               return Generate_Bool_Statement (Acc);

            when TYPE_STRING              =>
               return Generate_String_Statement (Acc);

            when TYPE_FLOAT | TYPE_DOUBLE =>
               return
                 Generate_Float_Statement
                   (Acc, Field.PB_Type.Value = TYPE_FLOAT);

            when TYPE_INT32
               | TYPE_UINT32
               | TYPE_SINT32
               | TYPE_FIXED32
               | TYPE_SFIXED32            =>
               return Generate_Integer_Statement (Acc);

            when TYPE_FIXED64
               | TYPE_SFIXED64
               | TYPE_SINT64
               | TYPE_INT64
               | TYPE_UINT64              =>
               return Generate_Int64_Statement (Acc);

            when TYPE_BYTES               =>
               return Generate_Bytes_Statement (Acc);

            when others                   =>
               return Generate_Null_Statement;
         end case;
      end if;
      return null;
   end Generate_Field;

   function Specification_Text
     (Self    : Google.Protobuf.Descriptor.File_Descriptor_Proto;
      Request : Google.Protobuf.Compiler.Plugin.Code_Generator_Request)
      return League.Strings.Universal_String
   is
      use type League.Strings.Universal_String;
      LF     : constant League.Strings.Universal_String :=
        +("" & Ada.Characters.Wide_Wide_Latin_1.LF);
      Pkg    : constant League.Strings.Universal_String := Package_Name (Self);
      Prefix : constant League.Strings.Universal_String := Get_Prefix (Self);

      function Generate_Spec
        (Msg    : Google.Protobuf.Descriptor.Descriptor_Proto;
         Prefix : League.Strings.Universal_String)
         return Ada_Pretty.Node_Access;

      function Generate_Spec
        (Msg    : Google.Protobuf.Descriptor.Descriptor_Proto;
         Prefix : League.Strings.Universal_String)
         return Ada_Pretty.Node_Access
      is
         Key    : constant League.Strings.Universal_String :=
           Compiler.Context.Join (Prefix, Msg.Name);
         Name   : constant League.Strings.Universal_String :=
           Compiler.Context.Named_Types (Key).Ada_Type.Type_Name;
         Result : Ada_Pretty.Node_Access := null;
         Nested : Ada_Pretty.Node_Access := null;
      begin
         if not Compiler.Context.Named_Types (Key).Is_Enumeration then
            Result :=
              F.New_Subprogram_Declaration
                (F.New_Subprogram_Specification
                   (Name       => F.New_Name (+"Write"),
                    Parameters =>
                      F.New_List
                        (F.New_Parameter
                           (Name            => F.New_Name (+"Stream"),
                            Type_Definition =>
                              F.New_Selected_Name
                                (+"PB_Support.JSON.JSON_Writer"),
                            Is_In           => True,
                            Is_Out          => True),
                         F.New_Parameter
                           (Name            => F.New_Name (+"Value"),
                            Type_Definition =>
                              F.New_Selected_Name
                                (+"Standard." & Pkg & "." & Name)))));
         end if;

         for J in 1 .. Msg.Nested_Type.Length loop
            Nested := Generate_Spec (Msg.Nested_Type (J), Key);
            if Nested /= null then
               Result :=
                 (if Result = null
                  then Nested
                  else F.New_List (Result, Nested));
            end if;
         end loop;
         return Result;
      end Generate_Spec;

      Specs  : Ada_Pretty.Node_Access := null;
      Nested : Ada_Pretty.Node_Access := null;
   begin
      for J in 1 .. Self.Message_Type.Length loop
         Nested := Generate_Spec (Self.Message_Type (J), Prefix);
         if Nested /= null then
            Specs :=
              (if Specs = null then Nested else F.New_List (Specs, Nested));
         end if;
      end loop;

      declare
         Unit : constant Ada_Pretty.Node_Access :=
           F.New_Compilation_Unit
             (Root    =>
                F.New_Package
                  (Name => F.New_Name (Pkg & ".JSON"), Public_Part => Specs),
              Clauses => F.New_With (F.New_Name (+"PB_Support.JSON")),
              License => Header_Comment (Self, Request));
      begin
         return League.Strings."&" (F.To_Text (Unit).Join (LF), LF);
      end;
   end Specification_Text;

   function Body_Text
     (Self    : Google.Protobuf.Descriptor.File_Descriptor_Proto;
      Request : Google.Protobuf.Compiler.Plugin.Code_Generator_Request)
      return League.Strings.Universal_String
   is
      use type League.Strings.Universal_String;
      LF     : constant League.Strings.Universal_String :=
        +("" & Ada.Characters.Wide_Wide_Latin_1.LF);
      Pkg    : constant League.Strings.Universal_String := Package_Name (Self);
      Prefix : constant League.Strings.Universal_String := Get_Prefix (Self);

      function Generate_Body
        (Msg    : Google.Protobuf.Descriptor.Descriptor_Proto;
         Prefix : League.Strings.Universal_String)
         return Ada_Pretty.Node_Access;

      function Generate_Repeated_Field
        (Msg           : Google.Protobuf.Descriptor.Descriptor_Proto;
         Key           : League.Strings.Universal_String;
         Field         : Google.Protobuf.Descriptor.Field_Descriptor_Proto;
         Ada_Name      : League.Strings.Universal_String;
         Json_Key      : League.Strings.Universal_String;
         Is_Message    : Boolean;
         Is_JSON_Array : Boolean) return Ada_Pretty.Node_Access;

      function Generate_Repeated_Field
        (Msg           : Google.Protobuf.Descriptor.Descriptor_Proto;
         Key           : League.Strings.Universal_String;
         Field         : Google.Protobuf.Descriptor.Field_Descriptor_Proto;
         Ada_Name      : League.Strings.Universal_String;
         Json_Key      : League.Strings.Universal_String;
         Is_Message    : Boolean;
         Is_JSON_Array : Boolean) return Ada_Pretty.Node_Access
      is
         use all type Compiler.Field_Descriptors.Option_Kind;
         use all type Google.Protobuf.Descriptor.PB_Type;

         Map_Index   : Natural := 0;
         Field_Stmts : Ada_Pretty.Node_Access := null;

         function Generate_Default_Map_Key_Statement
           (Field : Google.Protobuf.Descriptor.Field_Descriptor_Proto)
            return Ada_Pretty.Node_Access;

         function Generate_Default_Map_Value_Statement
           (Field : Google.Protobuf.Descriptor.Field_Descriptor_Proto)
            return Ada_Pretty.Node_Access;

         function Generate_Map_Key_Statement
           (Field : Google.Protobuf.Descriptor.Field_Descriptor_Proto;
            Acc   : League.Strings.Universal_String)
            return Ada_Pretty.Node_Access;

         function Generate_Map_Value_Statement
           (Pkg   : League.Strings.Universal_String;
            Field : Google.Protobuf.Descriptor.Field_Descriptor_Proto;
            Acc   : League.Strings.Universal_String)
            return Ada_Pretty.Node_Access;

         function Generate_Default_Map_Key_Statement
           (Field : Google.Protobuf.Descriptor.Field_Descriptor_Proto)
            return Ada_Pretty.Node_Access is
         begin
            --  Map entries are emitted when the key is present, even if the
            --  value itself is the type default.

            if Field.PB_Type.Is_Set then
               case Field.PB_Type.Value is
                  when TYPE_BOOL                                =>
                     return
                       F.New_Statement
                         (F.New_Apply
                            (F.New_Selected_Name (+"Stream.Write_Map_Key"),
                             F.New_Name (+"False")));

                  when TYPE_INT32 | TYPE_SINT32 | TYPE_SFIXED32 =>
                     return
                       F.New_Statement
                         (F.New_Apply
                            (F.New_Selected_Name (+"Stream.Write_Map_Key"),
                             F.New_Name (+"Interfaces.Integer_32'(0)")));

                  when TYPE_UINT32 | TYPE_FIXED32               =>
                     return
                       F.New_Statement
                         (F.New_Apply
                            (F.New_Selected_Name (+"Stream.Write_Map_Key"),
                             F.New_Name (+"Interfaces.Unsigned_32'(0)")));

                  when TYPE_INT64 | TYPE_SINT64 | TYPE_SFIXED64 =>
                     return
                       F.New_Statement
                         (F.New_Apply
                            (F.New_Selected_Name (+"Stream.Write_Map_Key"),
                             F.New_Name (+"Interfaces.Integer_64'(0)")));

                  when TYPE_UINT64 | TYPE_FIXED64               =>
                     return
                       F.New_Statement
                         (F.New_Apply
                            (F.New_Selected_Name (+"Stream.Write_Map_Key"),
                             F.New_Name (+"Interfaces.Unsigned_64'(0)")));

                  when others                                   =>
                     null;
               end case;
            end if;

            return
              F.New_Statement
                (F.New_Apply
                   (F.New_Selected_Name (+"Stream.Write_Key"),
                    F.New_String_Literal (+"")));
         end Generate_Default_Map_Key_Statement;

         function Generate_Map_Key_Statement
           (Field : Google.Protobuf.Descriptor.Field_Descriptor_Proto;
            Acc   : League.Strings.Universal_String)
            return Ada_Pretty.Node_Access
         is
            Is_Option : constant Compiler.Field_Descriptors.Option_Kind :=
              Compiler.Field_Descriptors.Is_Optional (Field);
            Cond      : constant Ada_Pretty.Node_Access :=
              F.New_Selected_Name (Acc & ".Is_Set");
            Present   : constant Ada_Pretty.Node_Access :=
              F.New_Statement
                (F.New_Apply
                   (F.New_Selected_Name (+"Stream.Write_Map_Key"),
                    F.New_Selected_Name (Acc & ".Value")));
         begin
            if Is_Option = Compiler.Field_Descriptors.Optional then
               return
                 F.New_If
                   (Cond,
                    Present,
                    Else_Path => Generate_Default_Map_Key_Statement (Field));
            end if;

            return
              F.New_Statement
                (F.New_Apply
                   (F.New_Selected_Name (+"Stream.Write_Map_Key"),
                    F.New_Selected_Name (Acc)));
         end Generate_Map_Key_Statement;

         function Generate_Default_Map_Value_Statement
           (Field : Google.Protobuf.Descriptor.Field_Descriptor_Proto)
            return Ada_Pretty.Node_Access is
         begin
            if Compiler.Field_Descriptors.Is_Message (Field) then
               return
                 F.New_List
                   (F.New_Statement
                      (F.New_Selected_Name (+"Stream.Start_Object")),
                    F.New_Statement
                      (F.New_Selected_Name (+"Stream.End_Object")));
            elsif Compiler.Field_Descriptors.Is_Enum (Field)
              and then Field.Type_Name.Is_Set
            then
               if Field.Type_Name.Value = +".google.protobuf.NullValue" then
                  return
                    F.New_Statement
                      (F.New_Selected_Name (+"Stream.Write_Null"));
               else
                  declare
                     Target : constant Compiler.Context.Named_Type :=
                       Compiler.Context.Named_Types (Field.Type_Name.Value);
                  begin
                     return
                       F.New_Statement
                         (F.New_Apply
                            (F.New_Selected_Name (+"Stream.Write_String"),
                             F.New_String_Literal (Target.Enum.Default)));
                  end;
               end if;
            elsif Field.PB_Type.Is_Set then
               case Field.PB_Type.Value is
                  when TYPE_BOOL                                =>
                     return
                       F.New_Statement
                         (F.New_Apply
                            (F.New_Selected_Name (+"Stream.Write_Boolean"),
                             F.New_Name (+"False")));

                  when TYPE_STRING | TYPE_BYTES                 =>
                     return
                       F.New_Statement
                         (F.New_Apply
                            (F.New_Selected_Name (+"Stream.Write_String"),
                             F.New_String_Literal (+"")));

                  when TYPE_FLOAT                               =>
                     return
                       F.New_Statement
                         (F.New_Apply
                            (F.New_Selected_Name (+"Stream.Write_Float"),
                             F.New_Apply
                               (F.New_Name (+"Interfaces.IEEE_Float_64"),
                                F.New_Name (+"0.0"))));

                  when TYPE_DOUBLE                              =>
                     return
                       F.New_Statement
                         (F.New_Apply
                            (F.New_Selected_Name (+"Stream.Write_Float"),
                             F.New_Name (+"0.0")));

                  when TYPE_INT32
                     | TYPE_UINT32
                     | TYPE_SINT32
                     | TYPE_FIXED32
                     | TYPE_SFIXED32                            =>
                     return
                       F.New_Statement
                         (F.New_Apply
                            (F.New_Selected_Name (+"Stream.Write_Integer"),
                             F.New_Apply
                               (F.New_Name (+"Long_Long_Integer"),
                                F.New_Literal (0))));

                  when TYPE_INT64 | TYPE_SINT64 | TYPE_SFIXED64 =>
                     return
                       F.New_Statement
                         (F.New_Apply
                            (F.New_Selected_Name (+"Stream.Write_Integer"),
                             F.New_Name (+"Interfaces.Integer_64'(0)")));

                  when TYPE_UINT64 | TYPE_FIXED64               =>
                     return
                       F.New_Statement
                         (F.New_Apply
                            (F.New_Selected_Name (+"Stream.Write_Integer"),
                             F.New_Name (+"Interfaces.Unsigned_64'(0)")));

                  when others                                   =>
                     null;
               end case;
            end if;

            return
              F.New_Statement (F.New_Selected_Name (+"Stream.Write_Null"));
         end Generate_Default_Map_Value_Statement;

         function Generate_Map_Value_Statement
           (Pkg   : League.Strings.Universal_String;
            Field : Google.Protobuf.Descriptor.Field_Descriptor_Proto;
            Acc   : League.Strings.Universal_String)
            return Ada_Pretty.Node_Access
         is
            Is_Option : constant Compiler.Field_Descriptors.Option_Kind :=
              Compiler.Field_Descriptors.Is_Optional (Field);
            Cond      : constant Ada_Pretty.Node_Access :=
              F.New_Selected_Name (Acc & ".Is_Set");
         begin
            if Is_Option = Compiler.Field_Descriptors.Optional then
               return
                 F.New_If
                   (Cond,
                    Generate_Field (Pkg, Field, Acc & ".Value"),
                    Else_Path => Generate_Default_Map_Value_Statement (Field));
            end if;

            return Generate_Field (Pkg, Field, Acc);
         end Generate_Map_Value_Statement;
      begin
         for N in 1 .. Msg.Nested_Type.Length loop
            declare
               Nested      :
                 constant Google.Protobuf.Descriptor.Descriptor_Proto :=
                   Msg.Nested_Type (N);
               Nested_Name : constant League.Strings.Universal_String :=
                 Compiler.Context.Join (Key, Nested.Name);
            begin
               if Field.Type_Name.Is_Set
                 and then Field.Type_Name.Value = Nested_Name
                 and then Nested.Options.Is_Set
                 and then Nested.Options.Value.Map_Entry.Is_Set
                 and then Nested.Options.Value.Map_Entry.Value
               then
                  Map_Index := N;
                  exit;
               end if;
            end;
         end loop;

         Field_Stmts :=
           F.New_Statement
             (F.New_Apply
                (F.New_Selected_Name (+"Stream.Write_Key"),
                 F.New_String_Literal (Json_Key)));

         if Map_Index > 0 then
            declare
               Map_Entry   :
                 constant Google.Protobuf.Descriptor.Descriptor_Proto :=
                   Msg.Nested_Type (Map_Index);
               Key_Field   :
                 constant Google.Protobuf.Descriptor.Field_Descriptor_Proto :=
                   Map_Entry.Field (1);
               Value_Field :
                 constant Google.Protobuf.Descriptor.Field_Descriptor_Proto :=
                   Map_Entry.Field (2);
            begin
               Field_Stmts :=
                 F.New_List
                   (Field_Stmts,
                    F.New_Statement
                      (F.New_Selected_Name (+"Stream.Start_Object")));

               declare
                  Iter : constant Ada_Pretty.Node_Access :=
                    F.New_List
                      (F.New_Literal (1),
                       F.New_Infix
                         (+"..",
                          F.New_Selected_Name
                            (+"Value." & Ada_Name & ".Length")));
               begin
                  declare
                     Entry_Acc : constant League.Strings.Universal_String :=
                       +"Value." & Ada_Name & " (J)";
                     Value_Acc : constant League.Strings.Universal_String :=
                       Entry_Acc & ".Value";
                     Key_Acc   : constant League.Strings.Universal_String :=
                       Entry_Acc & ".Key";
                     Loop_Body : Ada_Pretty.Node_Access;
                  begin
                     Loop_Body :=
                       F.New_List
                         (Generate_Map_Key_Statement (Key_Field, Key_Acc),
                          Generate_Map_Value_Statement
                            (Pkg, Value_Field, Value_Acc));

                     Field_Stmts :=
                       F.New_List
                         (Field_Stmts,
                          F.New_For (F.New_Name (+"J"), Iter, Loop_Body));
                  end;
               end;

               Field_Stmts :=
                 F.New_List
                   (Field_Stmts,
                    F.New_Statement
                      (F.New_Selected_Name (+"Stream.End_Object")));
            end;
         else
            if Is_JSON_Array then
               Field_Stmts :=
                 F.New_List
                   (Field_Stmts,
                    F.New_Statement
                      (F.New_Selected_Name (+"Stream.Start_Array")));
            end if;

            declare
               Acc       : constant League.Strings.Universal_String :=
                 (if Field.PB_Type.Is_Set
                    and then not Is_Message
                    and then
                      not (Compiler.Context.Runtime_Dep
                           = Compiler.Runtime_League
                           and then
                             Field.PB_Type.Value
                             = Google.Protobuf.Descriptor.TYPE_STRING)
                  then +"Value." & Ada_Name & ".Get (J)"
                  else +"Value." & Ada_Name & " (J)");
               Loop_Body : constant Ada_Pretty.Node_Access :=
                 Generate_Field (Pkg, Field, Acc);
               Iter      : constant Ada_Pretty.Node_Access :=
                 F.New_List
                   (F.New_Literal (1),
                    F.New_Infix
                      (+"..",
                       F.New_Selected_Name
                         (+"Value." & Ada_Name & ".Length")));
            begin
               Field_Stmts :=
                 F.New_List
                   (Field_Stmts,
                    F.New_For (F.New_Name (+"J"), Iter, Loop_Body));
            end;

            if Is_JSON_Array then
               Field_Stmts :=
                 F.New_List
                   (Field_Stmts,
                    F.New_Statement
                      (F.New_Selected_Name (+"Stream.End_Array")));
            end if;
         end if;

         return Field_Stmts;
      end Generate_Repeated_Field;

      function Generate_Body
        (Msg    : Google.Protobuf.Descriptor.Descriptor_Proto;
         Prefix : League.Strings.Universal_String)
         return Ada_Pretty.Node_Access
      is
         use type Google.Protobuf.Descriptor.Label;
         Key    : constant League.Strings.Universal_String :=
           Compiler.Context.Join (Prefix, Msg.Name);
         Name   : constant League.Strings.Universal_String :=
           Compiler.Context.Named_Types (Key).Ada_Type.Type_Name;
         Result : Ada_Pretty.Node_Access := null;
         Stmts  : Ada_Pretty.Node_Access := null;

         procedure Process_Single_Field
           (Field         : Google.Protobuf.Descriptor.Field_Descriptor_Proto;
            Current_Stmts : in out Ada_Pretty.Node_Access);
         --  Helper procedure to process a single field and update Stmts

         procedure Process_Single_Field
           (Field         : Google.Protobuf.Descriptor.Field_Descriptor_Proto;
            Current_Stmts : in out Ada_Pretty.Node_Access)
         is
            use all type Compiler.Field_Descriptors.Option_Kind;
            F_Name        : constant League.Strings.Universal_String :=
              Field.Name.Value;
            Ada_Name      : constant League.Strings.Universal_String :=
              Compiler.Context.To_Ada_Name (Field.Name.Value);
            Json_Key      : constant League.Strings.Universal_String :=
              (if not Compiler.Context.Preserve_Proto_Field_Names and then
                  Field.Json_Name.Is_Set
               then Field.Json_Name.Value
               else F_Name);
            Is_Vector     : constant Boolean :=
              Compiler.Field_Descriptors.Is_Repeated
                (Field, Pkg, Name, Compiler.Context.Fake);
            Is_JSON_Array : constant Boolean :=
              Field.Label.Is_Set
              and then
                Field.Label.Value = Google.Protobuf.Descriptor.LABEL_REPEATED;
            Is_Option     : constant Compiler.Field_Descriptors.Option_Kind :=
              Compiler.Field_Descriptors.Is_Optional (Field);
            Is_Oneof      : constant Boolean :=
              Compiler.Field_Descriptors.Is_One_Of (Field);
            Is_Message    : constant Boolean :=
              Compiler.Field_Descriptors.Is_Message (Field);
            Field_Stmts   : Ada_Pretty.Node_Access := null;
         begin
            if Is_Vector then
               Field_Stmts :=
                 Generate_Repeated_Field
                   (Msg           => Msg,
                    Key           => Key,
                    Field         => Field,
                    Ada_Name      => Ada_Name,
                    Json_Key      => Json_Key,
                    Is_Message    => Is_Message,
                    Is_JSON_Array => Is_JSON_Array);

               Current_Stmts :=
                 F.New_List
                   (Current_Stmts,
                    F.New_If
                      (F.New_List
                         (F.New_Selected_Name
                            (+"Value." & Ada_Name & ".Length"),
                          F.New_Infix (+">", F.New_Literal (0))),
                       Field_Stmts));
            else
               Field_Stmts :=
                 F.New_Statement
                   (F.New_Apply
                      (F.New_Selected_Name (+"Stream.Write_Key"),
                       F.New_String_Literal (Json_Key)));

               declare
                  Acc : League.Strings.Universal_String :=
                    (if Is_Oneof
                     then +"Value.Variant." & Ada_Name
                     else +"Value." & Ada_Name);
               begin
                  if Is_Oneof then
                     declare
                        Selector : constant League.Strings.Universal_String :=
                          Compiler.Context.To_Ada_Name
                            (Msg.Oneof_Decl
                               (Natural (Field.Oneof_Index.Value) + 1)
                               .Name
                               .Value);
                     begin
                        Field_Stmts :=
                          F.New_List
                            (Field_Stmts, Generate_Field (Pkg, Field, Acc));
                        Current_Stmts :=
                          F.New_List
                            (Current_Stmts,
                             F.New_If
                               (F.New_List
                                  (F.New_Selected_Name
                                     (+"Value.Variant." & Selector),
                                   F.New_Infix
                                     (+"=", F.New_Name (Ada_Name & "_Kind"))),
                                Field_Stmts));
                     end;
                  elsif Is_Option = Optional then
                     declare
                        Cond : constant Ada_Pretty.Node_Access :=
                          F.New_Selected_Name (Acc & ".Is_Set");
                     begin
                        Acc.Append (+".Value");
                        Field_Stmts :=
                          F.New_List
                            (Field_Stmts, Generate_Field (Pkg, Field, Acc));
                        Current_Stmts :=
                          F.New_List
                            (Current_Stmts, F.New_If (Cond, Field_Stmts));
                     end;
                  else
                     Field_Stmts :=
                       F.New_List
                         (Field_Stmts, Generate_Field (Pkg, Field, Acc));
                     Current_Stmts := F.New_List (Current_Stmts, Field_Stmts);
                  end if;
               end;
            end if;
         end Process_Single_Field;

      begin
         if not Compiler.Context.Named_Types (Key).Is_Enumeration then
            if Pkg & "." & Name = +"Google.Protobuf.Timestamp.Timestamp" then
               Stmts :=
                 F.New_Statement
                   (F.New_Apply
                      (F.New_Selected_Name
                         (+"PB_Support.JSON.Write_Timestamp"),
                       F.New_List
                         (F.New_List
                            (F.New_Name (+"Stream"),
                             F.New_Selected_Name (+"Value.Seconds")),
                          F.New_Selected_Name (+"Value.Nanos"))));
            elsif Pkg & "." & Name = +"Google.Protobuf.Duration.Duration" then
               Stmts :=
                 F.New_Statement
                   (F.New_Apply
                      (F.New_Selected_Name (+"PB_Support.JSON.Write_Duration"),
                       F.New_List
                         (F.New_List
                            (F.New_Name (+"Stream"),
                             F.New_Selected_Name (+"Value.Seconds")),
                          F.New_Selected_Name (+"Value.Nanos"))));
            else
               Stmts :=
                 F.New_Statement
                   (F.New_Selected_Name (+"Stream.Start_Object"));

               for K in 1 .. Msg.Field.Length loop
                  Process_Single_Field (Msg.Field (K), Stmts);
               end loop;

               Stmts :=
                 F.New_List
                   (Stmts,
                    F.New_Statement
                      (F.New_Selected_Name (+"Stream.End_Object")));
            end if;

            Result :=
              F.New_Subprogram_Body
                (Specification =>
                   F.New_Subprogram_Specification
                     (Name       => F.New_Name (+"Write"),
                      Parameters =>
                        F.New_List
                          (F.New_Parameter
                             (Name            => F.New_Name (+"Stream"),
                              Type_Definition =>
                                F.New_Selected_Name
                                  (+"PB_Support.JSON.JSON_Writer"),
                              Is_In           => True,
                              Is_Out          => True),
                           F.New_Parameter
                             (Name            => F.New_Name (+"Value"),
                              Type_Definition =>
                                F.New_Selected_Name
                                  (+"Standard." & Pkg & "." & Name)))),
                 Statements    => Stmts);
         end if;

         for J in 1 .. Msg.Nested_Type.Length loop
            declare
               Nested : constant Ada_Pretty.Node_Access :=
                 Generate_Body (Msg.Nested_Type (J), Key);
            begin
               if Nested /= null then
                  Result :=
                    (if Result = null
                     then Nested
                     else F.New_List (Result, Nested));
               end if;
            end;
         end loop;
         return Result;
      end Generate_Body;

      Body_Stmts : Ada_Pretty.Node_Access := null;
      Unit       : Ada_Pretty.Node_Access;
   begin
      for J in 1 .. Self.Message_Type.Length loop
         declare
            Nested : constant Ada_Pretty.Node_Access :=
              Generate_Body (Self.Message_Type (J), Prefix);
         begin
            if Nested /= null then
               Body_Stmts :=
                 (if Body_Stmts = null
                  then Nested
                  else F.New_List (Body_Stmts, Nested));
            end if;
         end;
      end loop;

      declare
         Clauses : Ada_Pretty.Node_Access := null;
      begin
         for J in 1 .. Self.Dependency.Length loop
            declare
               Item   :
                 constant Google.Protobuf.Descriptor.File_Descriptor_Proto :=
                   Compiler.Context.Get_File
                     (Request, Self.Dependency.Element (J));
               Name   : constant League.Strings.Universal_String :=
                 Package_Name (Item) & ".JSON";
               Clause : constant Ada_Pretty.Node_Access :=
                 F.New_With (F.New_Name (Name));
            begin
               if Clauses = null then
                  Clauses := Clause;
               else
                  Clauses := F.New_List (Clauses, Clause);
               end if;
            end;
         end loop;

         declare
            New_Body : Ada_Pretty.Node_Access := Body_Stmts;
         begin
            if Compiler.Context.Runtime_Dep /= Compiler.Runtime_League then
               New_Body :=
                 F.New_List
                   (F.New_Subprogram_Declaration
                      (Specification =>
                         F.New_Subprogram_Specification
                           (Name       => F.New_Name (+"""+"""),
                            Parameters =>
                              F.New_Parameter
                                (Name            => F.New_Name (+"Text"),
                                 Type_Definition =>
                                   F.New_Name
                                     (+"Ada.Strings.Unbounded."
                                      & "Unbounded_String")),
                            Result     => F.New_Name (+"String")),
                       Renamed       =>
                         (F.New_Name (+"Ada.Strings.Unbounded.To_String"))),
                    Body_Stmts);

               declare
                  U_Clause : constant Ada_Pretty.Node_Access :=
                    F.New_With (F.New_Name (+"Ada.Strings.Unbounded"));
               begin
                  if Clauses = null then
                     Clauses := U_Clause;
                  else
                     Clauses := F.New_List (Clauses, U_Clause);
                  end if;
               end;
            else
               New_Body :=
                 F.New_List
                   (F.New_Subprogram_Declaration
                      (Specification =>
                         F.New_Subprogram_Specification
                           (Name       => F.New_Name (+"""+"""),
                            Parameters =>
                              F.New_Parameter
                                (Name            => F.New_Name (+"Text"),
                                 Type_Definition =>
                                   F.New_Name
                                     (+"League.Strings.Universal_String"
                                      & "'Class")),
                            Result     => F.New_Name (+"String")),
                       Renamed       =>
                         F.New_Selected_Name
                           (+"League.Strings.To_UTF_8_String")),
                    Body_Stmts);
               declare
                  U_Clause : constant Ada_Pretty.Node_Access :=
                    F.New_With (F.New_Name (+"League.Strings"));
               begin
                  if Clauses = null then
                     Clauses := U_Clause;
                  else
                     Clauses := F.New_List (Clauses, U_Clause);
                  end if;
               end;
            end if;

            Unit :=
              F.New_Compilation_Unit
                (Root    =>
                   F.New_Package_Body
                     (Name => F.New_Name (Pkg & ".JSON"), List => New_Body),
                 Clauses => Clauses,
                 License => Header_Comment (Self, Request));
         end;
      end;
      return League.Strings."&" (F.To_Text (Unit).Join (LF), LF);
   end Body_Text;

end Compiler.File_Descriptors.JSON;
