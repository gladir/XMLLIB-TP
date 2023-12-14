{ @author: Sylvain Maltais (support@gladir.com)
  @created: 2023
  @website(https://www.gladir.com/CODER/XMLLIB-TP)
  @abstract(Target: Turbo Pascal 7, Free Pascal 3.2)
}

Unit XMLMAKE;

INTERFACE

Procedure BeginTag(Var F:Text;TagName:String);
Function  CloseXML(Var F:Text):Boolean;
Function  CreateXML(Var F:Text;FileName:String):Boolean;
Procedure EndTag(Var F:Text;TagName:String);
Procedure WriteDataTag(Var F:Text;Value:String);
Procedure WriteTag(Var F:Text;TagName,Value:String);

IMPLEMENTATION

Function CreateXML(Var F:Text;FileName:String):Boolean;
Var
 R:Boolean;
Begin
 {$I-}Assign(F,FileName);
 Rewrite(F);{$I+}
 R:=IOResult=0;
 CreateXML:=R;
 If(R)Then Begin
  WriteLn(F,'<?xml version="1.0" encoding="UTF-8"?>');
 End;
End;

Procedure BeginTag(Var F:Text;TagName:String);Begin
 Write(F,'<',TagName,'>');
End;

Procedure WriteDataTag(Var F:Text;Value:String);Begin
 Write(F,Value);
End;

Procedure WriteTag(Var F:Text;TagName,Value:String);Begin
 WriteLn(F,'<',TagName,'>',Value,'</',TagName,'>');
End;

Procedure EndTag(Var F:Text;TagName:String);Begin
 WriteLn(F,'</',TagName,'>');
End;

Function CloseXML(Var F:Text):Boolean;Begin
 Close(F);
 CloseXML:=IOResult=0;
End;

END.