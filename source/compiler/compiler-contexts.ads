with Ada.Containers.Hashed_Maps;

with Google_Protobuf.Compiler.CodeGeneratorRequest;
with Google_Protobuf.FileDescriptorProto;

with League.Strings.Hash;

with Ada_Pretty;

package Compiler.Contexts is

   X : Google_Protobuf.Compiler.CodeGeneratorRequest.Instance;
   F : aliased Ada_Pretty.Factory;

   type Ada_Type_Info is record
      Package_Name : League.Strings.Universal_String;
      Type_Name    : League.Strings.Universal_String;
   end record;

   package String_Hash_Maps is new Ada.Containers.Hashed_Maps
     (League.Strings.Universal_String,
      Ada_Type_Info,
      League.Strings.Hash,
      League.Strings."=");

   Type_Map : String_Hash_Maps.Map;

   procedure Populate_Type_Map;

--     Done        : String_Hash_Sets.Set;
--     In_Progress : String_Hash_Sets.Set;

   function Find_FileDescriptorProto
     (Name : String)
      return Google_Protobuf.FileDescriptorProto.FileDescriptorProto_Access;

end Compiler.Contexts;
