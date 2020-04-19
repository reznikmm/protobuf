package body Compiler.Contexts is

   ------------------------------
   -- Find_FileDescriptorProto --
   ------------------------------

   function Find_FileDescriptorProto (Name : String)
      return Google_Protobuf.FileDescriptorProto.FileDescriptorProto_Access
   is
      Result : Google_Protobuf.FileDescriptorProto.FileDescriptorProto_Access;
   begin
      for J in 0 .. X.Proto_File_Size - 1 loop
         Result := X.Get_Proto_File (J);

         if Result.Get_Name = Name then
            return Result;
         end if;
      end loop;

      return null;
   end Find_FileDescriptorProto;

end Compiler.Contexts;
