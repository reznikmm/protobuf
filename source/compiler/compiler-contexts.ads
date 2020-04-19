--  with Ada.Containers.Hashed_Sets;

with Google_Protobuf.Compiler.CodeGeneratorRequest;
with Google_Protobuf.FileDescriptorProto;

--  with League.Strings.Hash;

with Ada_Pretty;

package Compiler.Contexts is

   X : Google_Protobuf.Compiler.CodeGeneratorRequest.Instance;
   F : aliased Ada_Pretty.Factory;

--     package String_Hash_Sets is new Ada.Containers.Hashed_Sets
--       (League.Strings.Universal_String,
--        League.Strings.Hash,
--        League.Strings."=",
--        League.Strings."=");
--
--     Done        : String_Hash_Sets.Set;
--     In_Progress : String_Hash_Sets.Set;

   function Find_FileDescriptorProto
     (Name : String)
      return Google_Protobuf.FileDescriptorProto.FileDescriptorProto_Access;

end Compiler.Contexts;
