	| sanatorio op |
	sanatorio:= Sanatorio new.
	sanatorio inicializa.
	[op ~= 0] whileTrue: [
	op := (Prompter prompt: 'MENU/ 1. Solicitar intervención 
	/ 2. Reporte de liquidaciones / 3. Carga / 0. Salir') asNumber.
	op = 1 ifTrue: [self altaIntervencion].
	op = 2 ifTrue: [self reporteLiquidaciones]
	op = 3 iftrue:[self cargaDatos]].

	