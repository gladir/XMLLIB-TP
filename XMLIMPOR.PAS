{ @author: Sylvain Maltais (support@gladir.com)
  @created: 2023
  @website(https://www.gladir.com/CODER/XMLLIB-TP)
  @abstract(Target: Turbo Pascal 7, Free Pascal 3.2)
}

Unit XMLImpor;

INTERFACE

Const
 errNoError=0;
 errCantCreateFile=-1;
 errCantOpenFile=-2;
 errCommandNotImplemented=-3;
 errCommandNotSupported=-4;
 errDataExpected=-5;
 errDataTypeExpected=-6;
 errIntoKeywordExpected=-7;
 errRecordTooBig=-8;
 errSeparatorExpected=-9;
 errStringWithoutTerminaison=-10;
 errTableNameExpected=-11;
 errTableKeywordExpected=-12;
 errUnknownCommand=-13;
 errUnknownDataType=-14;

Function CSV2XML(SourceName,TargetName:String):Integer;
Function GetErrorMessageImport(Err:Integer):String;
Function JSON2XML(SourceName,TargetName:String):Integer;
Function SQL2XML(SourceName,TargetName:String):Integer;

IMPLEMENTATION

Uses DOS;

Const
 CommandList:Array[0..5]of String[8]=(
  'CREATE','DELETE','INSERT','SELECT','SHOW','UPDATE'
 );

Type
 FieldRec=Record
  FieldName:String[50];
  FieldType:Char;
  Spare1,Spare2:Integer;
  Width,Dec:Byte;
 End;

Var
 SourceSQL,TargetXML:Text;
 CommandFound,FirstInsert:Boolean;
 FieldCount:Integer;
 CurrLine,CurrCommand,ParamList,TableName:String;
 J,ByteWrited:Integer;
 FieldList:Array[0..127]of FieldRec;

Function TrimL(S:String):String;
Var
 I:Byte;
Begin
 For I:=1to Length(S)do Begin
  If S[I]<>' 'Then Begin
   TrimL:=Copy(S,I,255);
   Exit;
  End;
 End;
 TrimL:=S;
End;

Function TrimR(s:String):String;
Var
 i:Integer;
Begin
 i:=Length(s);
 While (i>0)and(s[i]in[#9,' '])do Dec(i);
 s[0]:=Chr(i);
 TrimR:=S;
End;

Function Trim(s:String):String;Begin
 Trim:=TrimL(TrimR(s));
End;

Function PadRight(S:String;Space:Byte):String;
Var
 I:Byte;
Begin
 If Length(S)<Space Then For I:=Length(S)+1 to Space do S:=S+' ';
 PadRight:=S;
End;

Function StrToUpper(S:String):String;
Var
 I:Byte;
Begin
 For I:=1 to Length(S)do Begin
  If S[I] in['a'..'z']Then S[I]:=Chr(Ord(S[I])-32);
 End;
 StrToUpper:=S;
End;

Function Path2Name(S:String):String;
Var
 D:DirStr;
 N:NameStr;
 E:ExtStr;
Begin
 FSplit(S,D,N,E);
 Path2Name:=N;
End;

Function Path2Ext(S:String):String;
Var
 D:DirStr;
 N:NameStr;
 E:ExtStr;
Begin
 FSplit(S,D,N,E);
 Path2Ext:=E;
End;

Function Path2NoExt(S:String):String;
Var
 D:DirStr;
 N:NameStr;
 E:ExtStr;
Begin
 FSplit(S,D,N,E);
 Path2NoExt:=D+N;
End;

Procedure ExtractCommand;
Var
 I,J:Byte;
Begin
 For I:=1 to Length(CurrLine)do Begin
  If Not(CurrLine[I]in['A'..'Z','a'..'z','_','-','0'..'9'])Then Begin
   CurrCommand:=StrToUpper(Copy(CurrLine,1,I-1));
   ParamList:=TrimL(Copy(CurrLine,I,255));
   Exit;
  End;
 End;
 CurrCommand:=StrToUpper(CurrLine);
 ParamList:='';
End;

Function CreateCommand:Integer;
Var
 Step:(_None,_Open,_Sep,_Close);
 Finished:Boolean;
 CurrWord,CurrWord2:String;
 I:Byte;
 ByteWrited:Integer;
Begin
 CreateCommand:=0;
 Finished:=False;
 If ParamList=''Then ReadLn(SourceSQL,ParamList);
 CurrWord:='';
 I:=1;
 While(I<=Length(ParamList))and(ParamList[I]in['A'..'Z','a'..'z'])do Begin
  CurrWord:=CurrWord+ParamList[I];
  Inc(I);
 End;
 If StrToUpper(CurrWord)<>'TABLE'Then Begin
  CreateCommand:=errTableKeywordExpected;
  Exit;
 End;
 While(I<=Length(ParamList))and(ParamList[I]in[' ',#9])do Inc(I);
 CurrWord2:='';
 While(I<=Length(ParamList))and(ParamList[I]in['A'..'Z','a'..'z'])do Begin
  CurrWord2:=CurrWord2+ParamList[I];
  Inc(I);
 End;
 If CurrWord2<>''Then Begin
  TableName:=CurrWord2;
 End
  Else
 Begin
  CreateCommand:=errTableNameExpected;
  Exit;
 End;
 Step:=_None;
 Repeat
  Repeat
   While(I<=Length(ParamList))and(ParamList[I]in[' ',#9])do Inc(I);
   If(Step=_None)Then Begin
    If(I<=Length(ParamList))and(ParamList[I]='(')Then Begin
     Step:=_Open;
     Inc(I);
    End;
   End
    Else
   If(Step=_Sep)Then Begin
    If(I<=Length(ParamList))and(ParamList[I]=')')Then Finished:=True Else
    If(I<=Length(ParamList))and(ParamList[I]=',')Then Begin
     Inc(I);
     Step:=_Open;
    End
     Else
    Begin
     CreateCommand:=errSeparatorExpected;
    End;
   End
    Else
   Begin
    FillChar(CurrWord,SizeOf(CurrWord),#0);
    CurrWord:='';
    While(I<=Length(ParamList))and(ParamList[I]in['A'..'Z','a'..'z'])do Begin
     CurrWord:=CurrWord+ParamList[I];
     Inc(I);
    End;
    If CurrWord<>''Then Begin
     While(I<=Length(ParamList))and(ParamList[I]in[' ',#9])do Inc(I);
     CurrWord2:='';
     While(I<=Length(ParamList))and(ParamList[I]in['A'..'Z','a'..'z'])do Begin
      CurrWord2:=CurrWord2+ParamList[I];
      Inc(I);
     End;
     If CurrWord2<>''Then Begin
     FieldList[FieldCount].FieldName:=CurrWord;
      If StrToUpper(CurrWord2)='BYTE'Then  Begin
       FieldList[FieldCount].FieldType:='L';
      End
       Else
      If StrToUpper(CurrWord2)='TEXT'Then Begin
       FieldList[FieldCount].FieldType:='C';
       FieldList[FieldCount].Width:=255;
      End
       Else
      If StrToUpper(CurrWord2)='INTEGER'Then Begin
       FieldList[FieldCount].FieldType:='I';
      End
       Else
      If StrToUpper(CurrWord2)='SMALLINT'Then Begin
       FieldList[FieldCount].FieldType:='I';
      End
       Else
      If StrToUpper(CurrWord2)='VARCHAR'Then Begin
       FieldList[FieldCount].FieldType:='C';
       FieldList[FieldCount].Width:=255;
      End
       Else
      Begin
       CreateCommand:=errUnknownDataType;
       Exit;
      End;
      Inc(FieldCount);
      Step:=_Sep;
     End
      Else
     Begin
      CreateCommand:=errDataTypeExpected;
      Exit;
     End;
    End;
   End;
  Until(I>=Length(ParamList))or(Finished);
  If Not(Finished)Then Begin
   If EOF(SourceSQL)Then Finished:=True
    Else
   Begin
    ReadLn(SourceSQL,ParamList);
    I:=1;
  End;
 End;
 Until Finished;
End;

Function DeleteCommand:Integer;Begin
 DeleteCommand:=errCommandNotImplemented;
End;

Function InsertCommand:Integer;
Var
 Finished:Boolean;
 CurrWord,CurrWord2:String;
 I:Byte;
 Step:(_None,_Open,_Sep,_Close);
 CurrField,ByteWrited:Integer;
Begin
 Finished:=False;
 CurrField:=0;
 If ParamList=''Then ReadLn(SourceSQL,ParamList);
 CurrWord:='';
 I:=1;
 While(I<=Length(ParamList))and(ParamList[I]in['A'..'Z','a'..'z'])do Begin
  CurrWord:=CurrWord+ParamList[I];
  Inc(I);
 End;
 If StrToUpper(CurrWord)<>'INTO'Then Begin
  InsertCommand:=errIntoKeywordExpected;
  Exit;
 End;
 Step:=_None;
 While(I<=Length(ParamList))and(ParamList[I]in[' ',#9])do Inc(I);
 CurrWord2:='';
 While(I<=Length(ParamList))and(ParamList[I]in['A'..'Z','a'..'z'])do Begin
  CurrWord2:=CurrWord2+ParamList[I];
  Inc(I);
 End;
 If CurrWord2<>''Then Begin
  TableName:=CurrWord2;
 End;
 WriteLn(TargetXML,' ':4,'<line>');
 Repeat
  Repeat
   While(I<=Length(ParamList))and(ParamList[I]in[' ',#9])do Inc(I);
   If(Step=_None)Then Begin
    If(I<=Length(ParamList))and(ParamList[I]='(')Then Begin
     Step:=_Open;
     Inc(I);
    End;
   End
    Else
   If(Step=_Sep)Then Begin
    If(I<=Length(ParamList))and(ParamList[I]=')')Then Begin
     Inc(I);
     Step:=_Close;
     Finished:=True;
    End
     Else
    If(I<=Length(ParamList))and(ParamList[I]=',')Then Begin
     Inc(I);
     Step:=_Open;
    End
     Else
    Begin
     InsertCommand:=errSeparatorExpected;
     Exit;
    End;
   End
    Else
   Begin
    FillChar(CurrWord,SizeOf(CurrWord),#0);
    CurrWord:='';
    If(I<=Length(ParamList))and(ParamList[I]='''')Then Begin
     Inc(I);
     While(I<=Length(ParamList))and(ParamList[I]<>'''')do Begin
      CurrWord:=CurrWord+ParamList[I];
      Inc(I);
     End;
     If(I<=Length(ParamList))and(ParamList[I]='''')Then Begin
      Inc(I);
     End
      Else
     Begin
      InsertCommand:=errStringWithoutTerminaison;
      Exit;
     End;
     Step:=_Sep;
    End
     Else
    If(I<=Length(ParamList))and(ParamList[I]in['0'..'9'])Then Begin
     While(I<=Length(ParamList))and(ParamList[I]in['0'..'9'])do Begin
      CurrWord:=CurrWord+ParamList[I];
      Inc(I);
     End;
     Step:=_Sep;
    End
     Else
    Begin
     InsertCommand:=errDataExpected;
     Exit;
    End;
    Write(TargetXML,' ':8,'<',Trim(FieldList[CurrField].FieldName),'>');
    Write(TargetXML,CurrWord);
    WriteLn(TargetXML,'</',Trim(FieldList[CurrField].FieldName),'>');
    Inc(CurrField);
   End;
  Until(I>=Length(ParamList))or(Finished);
  If Not(Finished)Then Begin
   If EOF(SourceSQL)Then Finished:=True
    Else
   Begin
    ReadLn(SourceSQL,ParamList);
    I:=1;
   End;
  End;
 Until Finished;
 WriteLn(TargetXML,' ':4,'</line>');
 FirstInsert:=False;
End;

Function SelectCommand:Integer;Begin
 SelectCommand:=errCommandNotImplemented;
End;

Function ShowComamnd:Integer;Begin
 ShowComamnd:=errCommandNotImplemented;
End;

Function ShowCommand:Integer;Begin
 ShowCommand:=errCommandNotImplemented;
End;

Function SQL2XML(SourceName,TargetName:String):Integer;
Var
 R:Integer;
Begin
 SQL2XML:=0;
 TableName:='';
 FirstInsert:=True;
 FieldCount:=0;
 FillChar(FieldList,SizeOf(FieldList),0);
  {$I-}Assign(SourceSQL,SourceName);
 Reset(SourceSQL);{$I+}
 If IoResult<>0Then Begin
  SQL2XML:=errCantOpenFile;
  Exit;
 End;
 {$I-}Assign(TargetXML,TargetName);
 Rewrite(TargetXML);{$I+}
 If IoResult<>0Then Begin
  SQL2XML:=errCantCreateFile;
  Exit;
 End;
 WriteLn(TargetXML,'<?xml version="1.0" encoding="UTF-8"?>');
 WriteLn(TargetXML,'<table>');
 While Not EOF(SourceSQL)do Begin
  ReadLn(SourceSQL,CurrLine);
  ExtractCommand;
  CommandFound:=False;
  For J:=Low(CommandList) to High(CommandList) do Begin
   If CurrCommand=CommandList[J]Then Begin
    CommandFound:=True;
    Case(J)of
     0:Begin
      R:=CreateCommand;
      If R<>0 Then Begin
       SQL2XML:=R;
       Exit;
      End;
     End;
     1:Begin
      R:=DeleteCommand;
      If R<>0 Then Begin
       SQL2XML:=R;
       Exit;
      End;
     End;
     2:Begin
      R:=InsertCommand;
      If R<>0 Then Begin
       SQL2XML:=R;
       Exit;
      End;
     End;
     3:Begin
      R:=SelectCommand;
      If R<>0 Then Begin
       SQL2XML:=R;
       Exit;
      End;
     End;
     4:Begin
      R:=ShowComamnd;
      If R<>0 Then Begin
       SQL2XML:=R;
       Exit;
      End;
     End;
     5:Begin
      R:=ShowCommand;
      If R<>0 Then Begin
       SQL2XML:=R;
       Exit;
      End;
     End;
     Else Begin
      SQL2XML:=errUnknownCommand;
      Exit;
     End;
    End;
   End;
  End;
  If Not(CommandFound)Then Begin
   SQL2XML:=errCommandNotSupported;
   Exit;
  End;
 End;
 WriteLn(TargetXML,'</table>');
 Close(SourceSQL);
End;

Function CSV2XML(SourceName,TargetName:String):Integer;
Var
 SourceCSV:Text;
 First:Boolean;
 I:Integer;
 PosField,NumField:Integer;
 NumRecord:LongInt;
 CurrField,CurrWord,FileName,TFileName:String;
Begin
 CSV2XML:=0;
 NumRecord:=0;
 NumField:=0;
 FillChar(FieldList,SizeOf(FieldList),0);
 TableName:=Path2Name(SourceName);
 FileName:=FExpand(SourceName);
 If Path2Ext(FileName)=''Then FileName:=FileName+'.CSV';
 Assign(SourceCSV,FileName);
 {$I-}Reset(SourceCSV);{$I+}
 If IoResult<>0Then Begin
  CSV2XML:=errCantOpenFile;
  Exit;
 End;
 TFileName:=FExpand(TargetName);
 If Path2Ext(TFileName)=''Then TFileName:=TFileName+'.XML';
 Assign(TargetXML,TFileName);
 {$I-}Rewrite(TargetXML); {$I+}
 If IoResult<>0Then Begin
  CSV2XML:=errCantCreateFile;
  Close(SourceCSV);
  Exit;
 End;
 First:=True;
 WriteLn(TargetXML,'<?xml version="1.0" encoding="UTF-8"?>');
 WriteLn(TargetXML,'<table>');
 While Not EOF(SourceCSV)do Begin
  ReadLn(SourceCSV,CurrLine);
  If(First)Then Begin
   First:=False;
   CurrWord:='';
   For I:=1 to Length(CurrLine)do Begin
    If CurrLine[I]=','Then Begin
     If(CurrWord[1]='"')and(CurrWord[Length(CurrWord)]='"')Then Begin
      FieldList[NumField].FieldName:=Copy(CurrWord,2,Length(CurrWord)-2);
      Inc(NumField);
     End
      Else
     Begin
      FieldList[NumField].FieldName:=CurrWord;
      Inc(NumField);
     End;
     CurrWord:='';
    End
     Else
    CurrWord:=CurrWord+CurrLine[I];
   End;
   CurrField:=CurrWord;
   FieldList[NumField].FieldName:=CurrWord;
   Inc(NumField);
   If NumField>High(FieldList)Then Begin
    CSV2XML:=errRecordTooBig;
    Exit;
   End;
  End
   Else
  Begin
   WriteLn(TargetXML,' ':4,'<line>');
   PosField:=0;
   CurrWord:='';
   For I:=1 to Length(CurrLine)do Begin
    If CurrLine[I]=','Then Begin
     If(CurrWord[1]='"')and(CurrWord[Length(CurrWord)]='"')Then Begin
      Write(TargetXML,' ':8,'<',FieldList[PosField].FieldName,'>');
      Write(TargetXML,Copy(CurrWord,2,Length(CurrWord)-2));
      WriteLn(TargetXML,'</',FieldList[PosField].FieldName,'>');
     End
      Else
     Begin
      Write(TargetXML,' ':8,'<',FieldList[PosField].FieldName,'>');
      Write(TargetXML,CurrWord);
      WriteLn(TargetXML,'</',FieldList[PosField].FieldName,'>');
     End;
     CurrWord:='';
     Inc(PosField);
    End
     Else
    CurrWord:=CurrWord+CurrLine[I];
   End;
   Write(TargetXML,' ':8,'<',FieldList[PosField].FieldName,'>');
   Write(TargetXML,Copy(CurrWord,2,Length(CurrWord)-2));
   WriteLn(TargetXML,'</',FieldList[PosField].FieldName,'>');
   WriteLn(TargetXML,' ':4,'</line>');
  End;
 End;
 WriteLn(TargetXML,'</table>');
 Close(TargetXML);
 Close(SourceCSV);
End;

Function JSON2XML(SourceName,TargetName:String):Integer;
Var
 Mode:(Key,Value);
 DirectValue,InString:Boolean;
 SourceJSON,TargetXML:Text;
 Spacing:Word;
 C:Char;
 CurrWord,FileName,TFileName:String;
 Ident:Integer;
 GroupValue:Array[0..49]of Boolean;
Begin
 FileName:=FExpand(SourceName);
 If Path2Ext(FileName)=''Then FileName:=FileName+'.JSON';
 Assign(SourceJSON,FileName);
 {$I-}Reset(SourceJSON);{$I+}
 If IoResult<>0Then Begin
  JSON2XML:=errCantOpenFile;
  Exit;
 End;
 Spacing:=2;
 InString:=False;
 Ident:=0;
 CurrWord:='';
 Mode:=Key;
 FillChar(FieldList,SizeOf(FieldList),0);
 FillChar(GroupValue,SizeOf(GroupValue),0);
 TFileName:=FExpand(TargetName);
 If Path2Ext(TFileName)=''Then TFileName:=TFileName+'.XML';
 Assign(TargetXML,TFileName);
 {$I-}Rewrite(TargetXML); {$I+}
 If IoResult<>0Then Begin
  JSON2XML:=errCantCreateFile;
  Close(SourceJSON);
  Exit;
 End;
 WriteLn(TargetXML,'<?xml version="1.0" encoding="UTF-8"?>');
 While Not EOF(SourceJSON)do Begin
  Read(SourceJSON,C);
  If(InString)Then Begin
   If C='"'Then InString:=False
           Else CurrWord:=CurrWord+C;
  End
   Else
  If Not(C in[#0,#13,#10])Then Begin
   Case C of
    '"':Begin
     InString:=True;
     DirectValue:=False;
    End;
    '{':Begin
     If(Mode=Value)Then Begin
      WriteLn(TargetXML);
     End;
     If CurrWord=''Then GroupValue[Ident]:=True;
     Inc(Ident);
    End;
    '}':Begin
     If(Mode=Value)Then Begin
      If(DirectValue)Then Write(TargetXML,Trim(CurrWord))
                     Else Write(TargetXML,CurrWord);
      Mode:=Key;
     End;
     If Not(GroupValue[Ident])Then WriteLn(TargetXML,'</',FieldList[Ident].FieldName,'>');
     If Ident>0Then Begin
      Dec(Ident);
      If GroupValue[Ident]Then Begin
       If Ident<>0 Then Begin
        If Ident>1 Then Write(TargetXML,' ':Pred(Ident)*Spacing);
        WriteLn(TargetXML,'</',FieldList[Ident].FieldName,'>');
       End;
      End;
     End;
     Mode:=Key;
    End;
    ':':Begin
     FieldList[Ident].FieldName:=Trim(CurrWord);
     If Ident>1 Then Write(TargetXML,' ':Pred(Ident)*Spacing);
     Write(TargetXML,'<',FieldList[Ident].FieldName,'>');
     CurrWord:='';
     DirectValue:=True;
     Mode:=Value;
     GroupValue[Ident]:=False;
    End;
    ',':Begin
     If(Mode=Value)Then Begin
      If(DirectValue)Then Write(TargetXML,Trim(CurrWord))
                     Else Write(TargetXML,CurrWord);
      WriteLn(TargetXML,'</',FieldList[Ident].FieldName,'>');
      Mode:=Key;
     End;
     CurrWord:='';
     Mode:=Key;
    End;
    Else CurrWord:=CurrWord+C;
   End;
  End;
 End;
 Close(TargetXML);
 Close(SourceJSON);
End;

Function GetErrorMessageImport(Err:Integer):String;Begin
 Case Err of
  errNoError:GetErrorMessageImport:='';
  errCantCreateFile:GetErrorMessageImport:='Impossible de cr‚er le fichier';
  errCantOpenFile:GetErrorMessageImport:='Impossible d''ouvrir le fichier source';
  errCommandNotImplemented:GetErrorMessageImport:='Commande non impl‚ment‚';
  errCommandNotSupported:GetErrorMessageImport:='Commande non support‚';
  errDataExpected:GetErrorMessageImport:='Donn‚es attendue';
  errDataTypeExpected:GetErrorMessageImport:='Type de donn‚es attendu';
  errIntoKeywordExpected:GetErrorMessageImport:='Mot clef INTO attendu';
  errRecordTooBig:GetErrorMessageImport:='Enregistrement trop grand';
  errSeparatorExpected:GetErrorMessageImport:='S‚parateur attendu';
  errStringWithoutTerminaison:GetErrorMessageImport:='Chaine de caractŠres sans terminaison';
  errTableNameExpected:GetErrorMessageImport:='Nom de table attendu';
  errTableKeywordExpected:GetErrorMessageImport:='Mot clef TABLE attendu';
  errUnknownCommand:GetErrorMessageImport:='Command inconnu';
  errUnknownDataType:GetErrorMessageImport:='Type de donn‚es inconnu';
 End;
End;

END.