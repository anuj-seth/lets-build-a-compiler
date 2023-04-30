with Cradle;
with Parser;

procedure Main is
begin
   Parser.Init;
   Parser.Expression;
exception
   when Cradle.Halt_Exception =>
      null;
end Main;
