function findChildren(cursor, kind, depth, max)
   local children = cursor:children();
   local size = children:size();
   for i=0, size - 1, 1 do
      local child = children:at(i);
      local childKind = child:kind();
      if kind == childKind then
         return child;
      end
      if not depth or depth > 0 then
         local ret = findChild(child, kind, depth - 1);
         if ret then
            return ret
         end
      end

      if cursor then
         if kind and cursor:kind() == kind then
            return cursor
         end
         -- if cursor:kind() == mat
      end
   end
end