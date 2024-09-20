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
	'Core\Object Arts\Dolphin\Base\Dolphin'
	'Core\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Prompter').

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

Paciente guid: (GUID fromString: '{217f8510-1557-4bcb-b9a6-bda460760c44}')!
Paciente comment: ''!
!Paciente categoriesForClass!Kernel-Objects! !
!Paciente methodsFor!

apellido
	^apellido!

apellido: anObject
	apellido := anObject!

cargaDatos!

dni
	^dni!

dni: anObject
	dni := anObject!

nombre
	^nombre!

nombre: anObject
	nombre := anObject!

operaciones
	^operaciones!

operaciones: anObject
	operaciones := anObject!

operar!

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
nombre!accessing!private! !
nombre:!accessing!private! !
operaciones!accessing!private! !
operaciones:!accessing!private! !
operar!public! !
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
	| operacion paciente dni med |
	operacion:= Operacion new.
	operacion cargaDatos.
	paciente:= Paciente new.
	dni:= Prompter prompt:'Ingrese dni del paciente'.
	paciente:= pacientes detect:[:pacientes | pacientes dni = dni ] 
				ifNone: [pacientes cargaDatos ].
	med:= medicos detect:[:medico | medico especialidad = operacion especialidad]
			ifNone:[Transcript show:'La especialidad del médico no coincide con la ingresada'].
	(med ~= nil) ifTrue:[paciente operar: operacion].
		!

inicializa
	pacientes:= OrderedCollection new.
	medicos:= OrderedCollection new.
	intervenciones:= OrderedCollection new.!

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

cargarDatos!

traerDatos! !
!AltaComplejidad categoriesForMethods!
cargarDatos!public! !
traerDatos!public! !
!

!AltaComplejidad class methodsFor!

porcenAdicional
	^PorcenAdicional!

porcenAdicional: anObject
	PorcenAdicional := anObject! !
!AltaComplejidad class categoriesForMethods!
porcenAdicional!accessing!private! !
porcenAdicional:!accessing!private! !
!

PacienteConObra guid: (GUID fromString: '{8d99fb2f-b3a7-4406-8ac6-4db319d7375a}')!
PacienteConObra comment: ''!
!PacienteConObra categoriesForClass!Kernel-Objects! !
!PacienteConObra methodsFor!

cargaDatos!

montoCobertura
	^montoCobertura!

montoCobertura: anObject
	montoCobertura := anObject!

nombreObra
	^nombreObra!

nombreObra: anObject
	nombreObra := anObject!

operar!

reporteLiquidaciones!

traerDatos! !
!PacienteConObra categoriesForMethods!
cargaDatos!public! !
montoCobertura!accessing!private! !
montoCobertura:!accessing!private! !
nombreObra!accessing!private! !
nombreObra:!accessing!private! !
operar!public! !
reporteLiquidaciones!public! !
traerDatos!public! !
!

"Binary Globals"!

