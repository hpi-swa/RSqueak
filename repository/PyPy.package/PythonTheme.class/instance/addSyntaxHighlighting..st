as yet unclassified
addSyntaxHighlighting: theme
	"This was the former sub-dued highlighting.
	self create apply.
	"
	
	theme
		set: #color for: #TextAction to: Color aqua;
		
		set: #default for: #PythonTextStyler to: {Color black};
		set: #invalid for: #PythonTextStyler to: {Color black};
		set: #excessCode for: #PythonTextStyler to: {Color black};
		set: #comment for: #PythonTextStyler to: {Color cyan muchDarker};
		set: #unfinishedComment for: #PythonTextStyler to: {Color black muchDarker. TextEmphasis italic};
		set: #'$' for: #PythonTextStyler to: {Color black muchDarker};
		set: #character for: #PythonTextStyler to: {Color black muchDarker};
		set: #integer for: #PythonTextStyler to: {Color black muchDarker};
		set: #number for: #PythonTextStyler to: {Color black muchDarker};	
		set: #- for: #PythonTextStyler to: {Color black muchDarker};
		set: #symbol for: #PythonTextStyler to: {Color blue muchDarker};	
		set: #stringSymbol for: #PythonTextStyler to: {Color blue muchDarker};	
		set: #literalArray for: #PythonTextStyler to: {Color blue muchDarker};
		set: #string for: #PythonTextStyler to: {Color magenta muchDarker. TextEmphasis normal};
		set: #unfinishedString for: #PythonTextStyler to: {Color black. TextEmphasis normal};
		set: #assignment for: #PythonTextStyler to: {nil. TextEmphasis bold};
		set: #ansiAssignment for: #PythonTextStyler to: {nil. TextEmphasis bold};
		set: #literal for: #PythonTextStyler to: {nil. TextEmphasis italic};
		set: #keyword for: #PythonTextStyler to: {Color blue muchDarker};
		set: #binary for: #PythonTextStyler to: {Color blue muchDarker};	
		set: #unary for: #PythonTextStyler to: {Color blue muchDarker};
		set: #incompleteKeyword for: #PythonTextStyler to: {Color gray muchDarker. TextEmphasis underlined};
		set: #incompleteBinary for: #PythonTextStyler to: {Color gray muchDarker. TextEmphasis underlined};	
		set: #incompleteUnary for: #PythonTextStyler to: {Color gray muchDarker. TextEmphasis underlined};
		set: #undefinedKeyword for: #PythonTextStyler to: {Color black};
		set: #undefinedBinary for: #PythonTextStyler to: {Color black};	
		set: #undefinedUnary for: #PythonTextStyler to: {Color black};													
		set: #patternKeyword for: #PythonTextStyler to: {nil. TextEmphasis bold};
		set: #patternBinary for: #PythonTextStyler to: {nil. TextEmphasis bold};
		set: #patternUnary for: #PythonTextStyler to: {nil. TextEmphasis bold};	
		set: #self for: #PythonTextStyler to: {Color black muchDarker};
		set: #super for: #PythonTextStyler to: {Color black muchDarker}; 
		set: #true for: #PythonTextStyler to: {Color black muchDarker};
		set: #false for: #PythonTextStyler to: {Color black muchDarker};
		set: #nil for: #PythonTextStyler to: {Color black muchDarker};
		set: #thisContext for: #PythonTextStyler to: {Color black muchDarker};
		set: #return for: #PythonTextStyler to: {Color black muchDarker};
		set: #patternArg for: #PythonTextStyler to: {Color blue muchDarker};	
		set: #methodArg for: #PythonTextStyler to: {Color blue muchDarker};
		set: #blockPatternArg for: #PythonTextStyler to: {Color blue muchDarker};
		set: #blockArg for: #PythonTextStyler to: {Color blue muchDarker};
		set: #argument for: #PythonTextStyler to: {Color blue muchDarker};
		set: #blockArgColon for: #PythonTextStyler to: {Color black}; 
		set: #leftParenthesis for: #PythonTextStyler to: {Color black}; 
		set: #rightParenthesis for: #PythonTextStyler to: {Color black}; 
		set: #leftParenthesis1 for: #PythonTextStyler to: {Color green muchDarker}; 
		set: #rightParenthesis1 for: #PythonTextStyler to: {Color green muchDarker}; 
		set: #leftParenthesis2 for: #PythonTextStyler to: {Color magenta muchDarker}; 
		set: #rightParenthesis2 for: #PythonTextStyler to: {Color magenta muchDarker}; 
		set: #leftParenthesis3 for: #PythonTextStyler to: {Color black muchDarker}; 
		set: #rightParenthesis3 for: #PythonTextStyler to: {Color black muchDarker}; 
		set: #leftParenthesis4 for: #PythonTextStyler to: {Color green darker}; 
		set: #rightParenthesis4 for: #PythonTextStyler to: {Color green darker}; 
		set: #leftParenthesis5 for: #PythonTextStyler to: {Color orange darker}; 
		set: #rightParenthesis5 for: #PythonTextStyler to: {Color orange darker}; 
		set: #leftParenthesis6 for: #PythonTextStyler to: {Color magenta darker}; 
		set: #rightParenthesis6 for: #PythonTextStyler to: {Color magenta darker}; 
		set: #leftParenthesis7 for: #PythonTextStyler to: {Color blue}; 
		set: #rightParenthesis7 for: #PythonTextStyler to: {Color blue}; 
		set: #blockStart for: #PythonTextStyler to: {Color black}; 
		set: #blockEnd for: #PythonTextStyler to: {Color black}; 
		set: #blockStart1 for: #PythonTextStyler to: {Color green muchDarker}; 
		set: #blockEnd1 for: #PythonTextStyler to: {Color green muchDarker}; 
		set: #blockStart2 for: #PythonTextStyler to: {Color magenta muchDarker}; 
		set: #blockEnd2 for: #PythonTextStyler to: {Color magenta muchDarker}; 
		set: #blockStart3 for: #PythonTextStyler to: {Color black muchDarker}; 
		set: #blockEnd3 for: #PythonTextStyler to: {Color black muchDarker}; 
		set: #blockStart4 for: #PythonTextStyler to: {Color green darker}; 
		set: #blockEnd4 for: #PythonTextStyler to: {Color green darker}; 
		set: #blockStart5 for: #PythonTextStyler to: {Color orange darker}; 
		set: #blockEnd5 for: #PythonTextStyler to: {Color orange darker}; 
		set: #blockStart6 for: #PythonTextStyler to: {Color magenta darker}; 
		set: #blockEnd6 for: #PythonTextStyler to: {Color magenta darker}; 
		set: #blockStart7 for: #PythonTextStyler to: {Color blue}; 
		set: #blockEnd7 for: #PythonTextStyler to: {Color blue}; 																																																		
		set: #arrayStart for: #PythonTextStyler to: {Color black}; 
		set: #arrayEnd for: #PythonTextStyler to: {Color black}; 
		set: #arrayStart1 for: #PythonTextStyler to: {Color black}; 
		set: #arrayEnd1 for: #PythonTextStyler to: {Color black}; 
		set: #byteArrayStart for: #PythonTextStyler to: {Color black}; 
		set: #byteArrayEnd for: #PythonTextStyler to: {Color black}; 
		set: #byteArrayStart1 for: #PythonTextStyler to: {Color black}; 
		set: #byteArrayEnd1 for: #PythonTextStyler to: {Color black}; 
		set: #leftBrace for: #PythonTextStyler to: {Color black}; 
		set: #rightBrace for: #PythonTextStyler to: {Color black}; 
		set: #cascadeSeparator for: #PythonTextStyler to: {Color black}; 
		set: #statementSeparator for: #PythonTextStyler to: {Color black}; 
		set: #externalCallType for: #PythonTextStyler to: {Color black}; 
		set: #externalCallTypePointerIndicator for: #PythonTextStyler to: {Color black}; 
		set: #primitiveOrExternalCallStart for: #PythonTextStyler to: {Color black}; 
		set: #primitiveOrExternalCallEnd for: #PythonTextStyler to: {Color black};
		set: #methodTempBar for: #PythonTextStyler to: {Color gray}; 
		set: #blockTempBar for: #PythonTextStyler to: {Color gray};
		set: #blockArgsBar for: #PythonTextStyler to: {Color gray};
		set: #primitive for: #PythonTextStyler to: {Color green muchDarker. TextEmphasis bold};
		set: #pragmaKeyword for: #PythonTextStyler to: {Color green muchDarker. TextEmphasis bold};
		set: #pragmaUnary for: #PythonTextStyler to: {Color green muchDarker. TextEmphasis bold};
		set: #pragmaBinary for: #PythonTextStyler to: {Color green muchDarker. TextEmphasis bold};									
		set: #externalFunctionCallingConvention for: #PythonTextStyler to: {Color green muchDarker. TextEmphasis bold}; 
		set: #module for: #PythonTextStyler to: {Color green muchDarker. TextEmphasis bold};
		set: #blockTempVar for: #PythonTextStyler to: {Color gray};
		set: #blockPatternTempVar for: #PythonTextStyler to: {Color gray};
		set: #instVar for: #PythonTextStyler to: {Color black};
		set: #workspaceVar for: #PythonTextStyler to: {Color black};
		set: #undefinedIdentifier for: #PythonTextStyler to: {Color black};
		set: #incompleteIdentifier for: #PythonTextStyler to: {Color gray darker. {TextEmphasis italic. TextEmphasis underlined}};
		set: #tempVar for: #PythonTextStyler to: {Color gray darker};
		set: #patternTempVar for: #PythonTextStyler to: {Color gray darker};
		set: #poolConstant for: #PythonTextStyler to: {Color gray muchDarker};
		set: #classVar for: #PythonTextStyler to: {Color gray muchDarker};
		set: #globalVar for: #PythonTextStyler to: {Color black}.
		
	"And the text differ"
	theme	
		set: #insertTextAttributes for: #TextDiffBuilder to: { TextColor black };
		set: #removeTextAttributes for: #TextDiffBuilder to: { TextEmphasis struckOut. TextColor blue };
		set: #normalTextAttributes for: #TextDiffBuilder to: { TextEmphasis normal }.