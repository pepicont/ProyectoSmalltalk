	| sanatorio op |
	sanatorio:= Sanatorio new.
	sanatorio inicializa.
	sanatorio preCarga. "hace pre-carga con objetos"
	[op ~= 0] whileTrue: [
	op := (Prompter prompt: 'MENU/1. Solicitar intervención/2. Reporte de liquidaciones/3. Carga /0. Salir') asNumber.
	(op = 1) ifTrue: [sanatorio altaIntervencion].
	(op = 2) ifTrue: [sanatorio reporteLiquidaciones].
	(op = 3) ifTrue:[sanatorio cargaDatos].].
	



	
	