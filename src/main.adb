with Cradle;
with Parser;

procedure Main is
begin
   Parser.Init;
   Parser.Assignment;
exception
   when Cradle.Halt_Exception =>
      null;
end Main;
