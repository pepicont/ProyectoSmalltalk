| package |
package := Package name: 'tp'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #AltaComplejidad;
	add: #Intervencion;
	add: #Medico;
	add: #Operacion;
	add: #Paciente;
	add: #PacienteConObra;
	add: #Sanatorio;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: #(
	'..\Core\Object Arts\Dolphin\Base\Dolphin'
	'..\Core\Object Arts\Dolphin\Base\Dolphin Legacy Date & Time'
	'..\Core\Object Arts\Dolphin\Base\Dolphin Message Box'
	'..\Core\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Prompter').

package!

"Class Definitions"!

Object subclass: #Intervencion
	instanceVariableNames: 'codigo descripcion especialidad precio'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Medico
	instanceVariableNames: 'nombre apellido matricula especialidad condicion'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Operacion
	instanceVariableNames: 'fecha id intervencion medico pagado'
	classVariableNames: 'Identificador'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Paciente
	instanceVariableNames: 'nombre apellido dni telefono operaciones'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Sanatorio
	instanceVariableNames: 'medicos pacientes intervenciones'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Intervencion subclass: #AltaComplejidad
	instanceVariableNames: ''
	classVariableNames: 'PorcenAdicional'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Paciente subclass: #PacienteConObra
	instanceVariableNames: 'nombreObra montoCobertura'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

Intervencion guid: (GUID fromString: '{31a9d5b1-46d7-46e6-87c5-7f6ba8d4b47a}')!
Intervencion comment: ''!
!Intervencion categoriesForClass!Kernel-Objects! !
!Intervencion methodsFor!

cargaDatos
|codigo descripcion especialidad precio|
codigo := Prompter prompt: 'Ingrese el código de la intervención: '.
descripcion := Prompter prompt: 'Ingrese la descripcion de la intervención: '.
especialidad := Prompter prompt: 'Ingrese la especialidad de la intervención: '.
precio := Prompter prompt: 'Ingrese el precio de la intervención: '.




!

cargarDatos!

codigo
	^codigo!

codigo: anObject
	codigo := anObject!

descripcion
	^descripcion!

descripcion: anObject
	descripcion := anObject!

especialidad
	^especialidad!

especialidad: anObject
	especialidad := anObject!

precio
	^precio!

precio: anObject
	precio := anObject!

traerDatos! !
!Intervencion categoriesForMethods!
cargaDatos!public! !
cargarDatos!public! !
codigo!accessing!private! !
codigo:!accessing!private! !
descripcion!accessing!private! !
descripcion:!accessing!private! !
especialidad!accessing!private! !
especialidad:!accessing!private! !
precio!accessing!private! !
precio:!accessing!private! !
traerDatos!public! !
!

Medico guid: (GUID fromString: '{36eb3dec-f645-4ffa-97d3-a2a049e71ded}')!
Medico comment: ''!
!Medico categoriesForClass!Kernel-Objects! !
!Medico methodsFor!

apellido
	^apellido!

apellido: anObject
	apellido := anObject!

cargaDatos
nombre := Prompter prompt: 'Ingrese el nombre del médico a ingresar'.
apellido := Prompter prompt: 'Ingrese el apellido del médico a ingresar'.
matricula := Prompter prompt: 'Ingrese la matricula del médico a ingresar'. 
especialidad := Prompter prompt: 'Ingrese la especialidad del médico a ingresar'.
!

condicion
	^condicion!

condicion: anObject
	condicion := anObject!

especialidad
	^especialidad!

especialidad: anObject
	especialidad := anObject!

matricula
	^matricula!

matricula: anObject
	matricula := anObject!

nombre
	^nombre!

nombre: anObject
	nombre := anObject!

reporteLiquidaciones!

traerDatos! !
!Medico categoriesForMethods!
apellido!accessing!private! !
apellido:!accessing!private! !
cargaDatos!public! !
condicion!accessing!private! !
condicion:!accessing!private! !
especialidad!accessing!private! !
especialidad:!accessing!private! !
matricula!accessing!private! !
matricula:!accessing!private! !
nombre!accessing!private! !
nombre:!accessing!private! !
reporteLiquidaciones!public! !
traerDatos!public! !
!

Operacion guid: (GUID fromString: '{4b2822fc-12b4-4b5d-bc51-22f4dd7cf7c4}')!
Operacion comment: ''!
!Operacion categoriesForClass!Kernel-Objects! !
!Operacion methodsFor!

cargaDatos
fecha:= Date fromString: (Prompter prompt: 'Ingrese la fecha de la operación' ) .
id:= Identificador.
Operacion aumentoIdentificador.
intervencion:=Prompter prompt: 'Ingrese la operación a efectuarse: '.
medico:=Prompter prompt: 'Ingrese el médico que va a realizar la operación'.






!

fecha
	^fecha!

fecha: anObject
	fecha := anObject!

id
	^id!

id: anObject
	id := anObject!

intervencion
	^intervencion!

intervencion: anObject
	intervencion := anObject!

medico
	^medico!

medico: anObject
	medico := anObject!

pagado
	^pagado!

pagado: anObject
	pagado := anObject! !
!Operacion categoriesForMethods!
cargaDatos!public! !
fecha!accessing!private! !
fecha:!accessing!private! !
id!accessing!private! !
id:!accessing!private! !
intervencion!accessing!private! !
intervencion:!accessing!private! !
medico!accessing!private! !
medico:!accessing!private! !
pagado!accessing!private! !
pagado:!accessing!private! !
!

!Operacion class methodsFor!

aumentoIdentificador
Identificador:=Identificador+1.
! !
!Operacion class categoriesForMethods!
aumentoIdentificador!public! !
!

Paciente guid: (GUID fromString: '{217f8510-1557-4bcb-b9a6-bda460760c44}')!
Paciente comment: ''!
!Paciente categoriesForClass!Kernel-Objects! !
!Paciente methodsFor!

apellido
	^apellido!

apellido: anObject
	apellido := anObject!

cargaDatos
dni:=Prompter prompt: 'ingrese dni del paciente '.
nombre :=Prompter prompt: 'ingrese nombre del paciente '.
apellido :=Prompter prompt: 'ingrese apellido del paciente '.
telefono :=Prompter prompt: 'ingrese telefono del paciente '.
!

dni
	^dni!

dni: anObject
	dni := anObject!

inicializa
operaciones := OrderedCollection new. !

nombre
	^nombre!

nombre: anObject
	nombre := anObject!

operaciones
	^operaciones!

operaciones: anObject
	operaciones := anObject!

operar!

operar: anOperacion
self operaciones add: anOperacion.

!

reporteLiquidaciones!

telefono
	^telefono!

telefono: anObject
	telefono := anObject!

traerDatos! !
!Paciente categoriesForMethods!
apellido!accessing!private! !
apellido:!accessing!private! !
cargaDatos!public! !
dni!accessing!private! !
dni:!accessing!private! !
inicializa!public! !
nombre!accessing!private! !
nombre:!accessing!private! !
operaciones!accessing!private! !
operaciones:!accessing!private! !
operar!public! !
operar:!public! !
reporteLiquidaciones!public! !
telefono!accessing!private! !
telefono:!accessing!private! !
traerDatos!public! !
!

Sanatorio guid: (GUID fromString: '{14193734-aa95-4daa-be4c-41a45d316d8e}')!
Sanatorio comment: ''!
!Sanatorio categoriesForClass!Kernel-Objects! !
!Sanatorio methodsFor!

altaIntervencion
	| operacion paciente dni ob flag1 flag2 |
	dni:= Prompter prompt:'Ingrese dni del paciente'.
	paciente:= pacientes detect:[:pas| pas dni = dni ] "Busca el paciente del dni ingresado en la colección de pacientes"
				ifNone: [ ob:=Prompter prompt: '¿TIene obra social? 1-Si/2-No'. "Pregunta si el ingresado tiene o no obra social"
						(ob=1) ifTrue: [paciente:= PacienteConObra new] 
								ifFalse:[
									paciente:= Paciente new].
						paciente inicializa. "inicializa la ordered collection de operaciones del paciente"
						paciente cargaDatos."Carga los datos del paciente"
						pacientes add:paciente]. "Agrega el paciente registrado a la colección de pacientes del sanatorio"
	operacion:= Operacion new.
	operacion cargaDatos. "Carga el id, la fecha, la intervención y el médico"
	flag1:=0.
	flag2:=0.
	intervenciones do:[ :int | (int=operacion intervencion) ifTrue: [flag1:=1]]. "Busca la intervención cargada en la coleccion de intervenciones" "!!!!!!falta resolver el tema de la disponibilidad del médico!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
"!!!!!!falta resolver el tema de la disponibilidad del médico!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
"!!!!!!falta resolver el tema de la disponibilidad del médico!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
"!!!!!!falta resolver el tema de la disponibilidad del médico!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
"!!!!!!falta resolver el tema de la disponibilidad del médico!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"

	medicos do: [:medic | (medic=operacion medico) ifTrue:[flag2:=1]]. "Busca el médico cargado en la colección de medicos"
	(flag1=0) ifTrue: [MessageBox notify:'La intervención cargada no esta en la colección de intervenciones'].
	(flag2=0) ifTrue: [MessageBox notify: 'El médico ingresado no se encuentra en la colección de médicos' ].
	((flag1=1)and: [flag2=1] ) ifTrue: [paciente operar:operacion].
		!

cargaDatos
|opc paciente intervencion medico complejo|
opc:=(Prompter prompt: '¿Que quiere ingresar?/1-Ingresar Pacientes/2-Ingresar Medico/ 3-Ingresar Intervenciones') asNumber .
(opc=1) ifTrue: [
			paciente:=Paciente new.
			paciente inicializa.
			paciente cargaDatos.
			pacientes add:paciente.
			].
(opc=2) ifTrue: [
			medico:=Medico new.
			medico cargaDatos.
			medicos add: medico.
			].
(opc=3) ifTrue: [
			complejo:=Prompter prompt: '¿La intervención es de alta complejidad? 1-Si/2-No'.
			(complejo=1) ifTrue: [intervencion := AltaComplejidad new] ifFalse:[intervencion :=Intervencion new].
			intervencion cargaDatos.
			intervenciones add:intervencion.
			].


!

inicializa
	pacientes:= OrderedCollection new.
	medicos:= OrderedCollection new.
	intervenciones:= OrderedCollection new.
	AltaComplejidad inicializar.!

intervenciones
	^intervenciones!

intervenciones: anObject
	intervenciones := anObject!

medicos
	^medicos!

medicos: anObject
	medicos := anObject!

pacientes
	^pacientes!

pacientes: anObject
	pacientes := anObject! !
!Sanatorio categoriesForMethods!
altaIntervencion!public! !
cargaDatos!public! !
inicializa!public! !
intervenciones!accessing!private! !
intervenciones:!accessing!private! !
medicos!accessing!private! !
medicos:!accessing!private! !
pacientes!accessing!private! !
pacientes:!accessing!private! !
!

AltaComplejidad guid: (GUID fromString: '{01f3471a-934b-45bb-969d-5bd476675db6}')!
AltaComplejidad comment: ''!
!AltaComplejidad categoriesForClass!Kernel-Objects! !
!AltaComplejidad methodsFor!

cargaDatos
super cargaDatos.
precio:=precio + precio*PorcenAdicional.!

cargarDatos!

traerDatos! !
!AltaComplejidad categoriesForMethods!
cargaDatos!public! !
cargarDatos!public! !
traerDatos!public! !
!

!AltaComplejidad class methodsFor!

inicializar
 PorcenAdicional:= 20.!

porcenAdicional
	^PorcenAdicional!

porcenAdicional: anObject
	PorcenAdicional := anObject! !
!AltaComplejidad class categoriesForMethods!
inicializar!public! !
porcenAdicional!accessing!private! !
porcenAdicional:!accessing!private! !
!

PacienteConObra guid: (GUID fromString: '{8d99fb2f-b3a7-4406-8ac6-4db319d7375a}')!
PacienteConObra comment: ''!
!PacienteConObra categoriesForClass!Kernel-Objects! !
!PacienteConObra methodsFor!

cargaDatos
dni:=Prompter prompt: 'ingrese dni del paciente '.
nombre :=Prompter prompt: 'ingrese nombre del paciente '.
apellido :=Prompter prompt: 'ingrese apellido del paciente '.
telefono :=Prompter prompt: 'ingrese telefono del paciente '.
nombreObra :=Prompter prompt: 'ingrese obra social del paciente'.
montoCobertura :=(Prompter prompt: 'ingrese monto de cobertura de la obra del paciente') asNumber asInteger .


!

incializa 
operaciones := OrderedCollection new.
!

montoCobertura
	^montoCobertura!

montoCobertura: anObject
	montoCobertura := anObject!

nombreObra
	^nombreObra!

nombreObra: anObject
	nombreObra := anObject!

operar: anOperacion
self operar:anOperacion.!

reporteLiquidaciones!

traerDatos! !
!PacienteConObra categoriesForMethods!
cargaDatos!public! !
incializa!public! !
montoCobertura!accessing!private! !
montoCobertura:!accessing!private! !
nombreObra!accessing!private! !
nombreObra:!accessing!private! !
operar:!public! !
reporteLiquidaciones!public! !
traerDatos!public! !
!

"Binary Globals"!

