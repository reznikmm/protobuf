with Ada.Containers.Hashed_Maps;
with Ada.Containers.Ordered_Sets;

with Google_Protobuf.Compiler.CodeGeneratorRequest;
with Google_Protobuf.DescriptorProto;
with Google_Protobuf.FileDescriptorProto;

with League.Strings.Hash;

with Ada_Pretty;

package Compiler.Contexts is

   X : Google_Protobuf.Compiler.CodeGeneratorRequest.Instance;
   F : aliased Ada_Pretty.Factory;

   package String_Sets is new Ada.Containers.Ordered_Sets
     (League.Strings.Universal_String,
      "<" => League.Strings."<",
      "=" => League.Strings."=");

   type Ada_Type is record
      Package_Name : League.Strings.Universal_String;
      Type_Name    : League.Strings.Universal_String;
   end record;

   function "+" (Self : Ada_Type) return League.Strings.Universal_String;

   type Ada_Type_Info is record
      Package_Name : League.Strings.Universal_String;
      Type_Name    : League.Strings.Universal_String;
      Default      : League.Strings.Universal_String;
      Message      : Google_Protobuf.DescriptorProto.DescriptorProto_Access;
   end record;

   package String_Hash_Maps is new Ada.Containers.Hashed_Maps
     (League.Strings.Universal_String,
      Ada_Type_Info,
      League.Strings.Hash,
      League.Strings."=");

   Type_Map : String_Hash_Maps.Map;

   procedure Populate_Type_Map;

   function Find_FileDescriptorProto
     (Name : String)
      return Google_Protobuf.FileDescriptorProto.FileDescriptorProto_Access;

end Compiler.Contexts;
