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
	'..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin'
	'..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin Legacy Date & Time'
	'..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin Message Box'
	'..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Prompter').

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
	instanceVariableNames: 'fecha id intervencion medico pagado montoAPagar'
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
codigo := Prompter prompt: 'Ingrese el código de la intervención: '.
descripcion := Prompter prompt: 'Ingrese la descripcion de la intervención: '.
especialidad := Prompter prompt: 'Ingrese la especialidad de la intervención: '.
precio := Prompter prompt: 'Ingrese el precio de la intervención: '.




!

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

info 
|informacion|
informacion:='Codigo: ',codigo, String tab,'Descripción: ',descripcion,String tab,'Especialidad: ',especialidad,String tab,'Precio: ',precio.
^informacion!

precio
	^precio!

precio: anObject
	precio := anObject!

traerDatos! !
!Intervencion categoriesForMethods!
cargaDatos!public! !
codigo!accessing!private! !
codigo:!accessing!private! !
descripcion!accessing!private! !
descripcion:!accessing!private! !
especialidad!accessing!private! !
especialidad:!accessing!private! !
info!public! !
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

info
	|header|
	header:= 'Médico: ', nombre, ' ', apellido,String tab,' Matricula: ', matricula.
^header!

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
info!public! !
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

cargaDatos:anObject
fecha:= Date fromString: (Prompter prompt: 'Ingrese la fecha de la operación' ) .
id:= Identificador.
Operacion aumentoIdentificador.
intervencion:= anObject.
"intervencion:=Prompter prompt: 'Ingrese el código de la operación a efectuarse: '."
medico:=Prompter prompt: 'Ingrese nombre y apellido del médico que va a realizar la operación'.
pagado:= Prompter prompt: 'Ingrese 1-Pagado/2-Pendiente de pago'.






!

cargaDatos:anObject a: anotherObject
fecha:= Date fromString: (Prompter prompt: 'Ingrese la fecha de la operación dd/mm/aaaa' ) .
Operacion aumentoIdentificador.
id:= Identificador.
intervencion:= anObject.
montoAPagar:= intervencion precio.
"intervencion:=Prompter prompt: 'Ingrese el código de la operación a efectuarse: '."
medico:=anotherObject.
pagado:= Prompter prompt: 'Ingrese 1-Pagado/2-Pendiente de pago'.






!

fecha
	^fecha!

fecha: anObject
	fecha := anObject!

id
	^id!

id: anObject
	id := anObject!

info
	|header|
	header:= 'Identificador: ', id displayString, ' ', 'Fecha: ',fecha displayString, String cr,String tab, String tab,' Descripción: ', intervencion descripcion, String tab,'Monto: ', montoAPagar.
^header!

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
cargaDatos:!public! !
cargaDatos:a:!public! !
fecha!accessing!private! !
fecha:!accessing!private! !
id!accessing!private! !
id:!accessing!private! !
info!public! !
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
!

inicializar
	Identificador:=0.! !
!Operacion class categoriesForMethods!
aumentoIdentificador!public! !
inicializar!public! !
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

cargaDatos:anObject
dni:=anObject.
nombre :=Prompter prompt: 'ingrese nombre del paciente '.
apellido :=Prompter prompt: 'ingrese apellido del paciente '.
telefono :=Prompter prompt: 'ingrese telefono del paciente '.!

dni
	^dni!

dni: anObject
	dni := anObject!

info
	|header|
	header:= 'Nombre: ', nombre, ' ', apellido, ' Obra Social: ---'.
^header!

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
cargaDatos:!public! !
dni!accessing!private! !
dni:!accessing!private! !
info!public! !
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
	| operacion paciente dni ob flag1 flag2 inter inttemp med medint flag3|
	
	dni:= Prompter prompt:'Ingrese dni del paciente'.
	paciente:= pacientes detect:[:pas| pas dni = dni ] "Busca el paciente del dni ingresado en la colección de pacientes"
				ifNone: [ ob:=Prompter prompt: '¿Tiene obra social? 1-Si/2-No'. "Pregunta si el ingresado tiene o no obra social"
						(ob=1) ifTrue: [paciente:= PacienteConObra new] 
								ifFalse:[
									paciente:= Paciente new].
						paciente inicializa. "inicializa la ordered collection de operaciones del paciente" 
						paciente cargaDatos:dni."Carga los datos del paciente"
						pacientes add:paciente]. "Agrega el paciente registrado a la colección de pacientes del sanatorio"
	MessageBox notify: 'La información de los médicos y las intervenciones está disponible en el Transcript'.
	self muestra. "Muestra la informaciónd de todos los médicos y las intervenciones cargados"
	operacion:= Operacion new.
	inttemp:= Prompter prompt: 'Ingrese el código de la intervención a realizar'. "NUEVA LINEA"
	flag1:=1.
	flag2:=1.
	inter:= intervenciones detect:[:in | (in codigo)=inttemp ] ifNone: [flag1:=0].  "NUEVA LINEA"
	medint:= Prompter prompt:'Ingrese matricula del médico'.
	med:= medicos detect: [:med1 | (med1 matricula) = medint] ifNone:[flag2:=0].
	flag3:= self checkear: inter a: med.
	(flag3=0) ifTrue: [MessageBox notify: 'El médico ingresado no coincide con la especialidad de la intervención' ].
	(flag1=1) ifTrue:[operacion cargaDatos: inter a: med]. "Carga el id, la fecha, la intervención y el médico" "MODIFICACIÓN DE LINEA"
	"!!!!!!falta resolver el tema de la disponibilidad del médico!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
	(flag1=0) ifTrue: [MessageBox notify:'La intervención cargada no esta en la colección de intervenciones'].
	(flag2=0) ifTrue: [MessageBox notify: 'El médico ingresado no se encuentra en la colección de médicos' ].
	((flag1=1 and: [flag2=1]) and: [flag3=1])  ifTrue: [paciente operar:operacion].
		!

cargaDatos
|opc paciente intervencion medico complejo op|
opc:=(Prompter prompt: '1-Ingresar Pacientes/2-Ingresar Medico/ 3-Ingresar Intervenciones') asNumber .
(opc=1) ifTrue: [
			op:= Prompter prompt: 'El paciente tiene obra? 1-Si/2-No'.
			(op=1) ifTrue:[paciente:= PacienteConObra new] ifFalse:[paciente:= Paciente new].
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

checkear: anIntervention a: aDoctor
|especIntervencion especMedico rta|
especIntervencion:= anIntervention especialidad.
especMedico:=aDoctor especialidad.
(especIntervencion=especMedico) ifTrue: [rta:=1] ifFalse:[rta:=0].
^rta!

inicializa
	pacientes:= OrderedCollection new.
	medicos:= OrderedCollection new.
	intervenciones:= OrderedCollection new.
	AltaComplejidad inicializar. "PREGUNTAR A AGUS QUÉ ES ESTO"
	Operacion inicializar. "agregado"!

intervenciones
	^intervenciones!

intervenciones: anObject
	intervenciones := anObject!

medicos
	^medicos!

medicos: anObject
	medicos := anObject!

muestra
"Muestra medicos e intervenciones cargadas"
Transcript clear.
Transcript tab;tab;tab;show:'MEDICOS:';cr.
Transcript show:'----------------------------------------------------------------------------------------------------------------------------';cr.

medicos do:[:med| Transcript show: (med info) printString ;tab;show:'Especialidad: ';show: (med especialidad) printString;tab;cr ].
Transcript cr.
Transcript tab;tab;tab;show:'INTERVENCIONES';cr.
Transcript show:'----------------------------------------------------------------------------------------------------------------------------';cr.
intervenciones do:[:int|  Transcript show: (int info) printString;cr ].


!

pacientes
	^pacientes!

pacientes: anObject
	pacientes := anObject!

preCarga
|int med pac|
int:=Intervencion new.
int codigo: '1'.
int descripcion: 'Apertura de rodilla'.
int especialidad: 'traumatologia'.
int precio: '1000'.
intervenciones add: int. "carga una intervención"

int:=Intervencion new.
int codigo: '2'.
int descripcion: 'Operación de corazón'.
int especialidad: 'cardiologia'.
int precio: '10000'.
intervenciones add: int.  "Hasta acá cargamos dos intervenciones por defecto para no andar cargando a mano cada vez que"
						"queremos probar el sistema"
med:=Medico new.
med nombre: 'Stefano'.
med apellido: 'Conti'.
med matricula:'52850'. 
med especialidad: 'traumatologia'. 
med condicion: '2'. "condición=2 significa médico no disponible" 
medicos add: med. "Cargamos 1 médico"

med:=Medico new.
med nombre: 'Agustin'.
med apellido: 'Dana'.
med matricula:'52935'. 
med especialidad: 'cardiologia'. 
med condicion: '1'. "condición=1 significa médico  disponible" 
medicos add: med. "Cargamos otro médico"

pac:=PacienteConObra new.
pac nombre:'Agustin'. 
pac apellido:'Dana'.
pac inicializa.
pac nombreObra:'osde'.
pac montoCobertura:'500' asNumber asInteger .
pac dni:'45949176'.
pac telefono:'3413946996'.
pacientes add:pac.  "Cargamos un paciente"

pac:=Paciente new.
pac nombre:'Stefano'. 
pac apellido:'Conti'.
pac inicializa.
pac dni:'46132662'.
pac telefono:'3411233443'.
pacientes add:pac. "Cargamos otro paciente"

 


!

reporteLiquidaciones
	Transcript clear.
	pacientes do:[:paciente | (paciente operaciones) do:[:operacion | ((operacion pagado='2')) ifTrue:[Transcript show: paciente info printString;cr;tab;show: operacion info printString;cr;tab;tab;tab; show:operacion medico info printString;cr;cr]]].
	"Recorre la colección pacientes, luego dentro de pacientes recorre sus operaciones y si la operación no está paga, armamos un header con la info del paciente, medico y la operacion por separado y printeamos todo con TS."
	"Operacion medico accede al objeto medico dentro de operación y info hace el header con la info del med"! !
!Sanatorio categoriesForMethods!
altaIntervencion!public! !
cargaDatos!public! !
checkear:a:!public! !
inicializa!public! !
intervenciones!accessing!private! !
intervenciones:!accessing!private! !
medicos!accessing!private! !
medicos:!accessing!private! !
muestra!public! !
pacientes!accessing!private! !
pacientes:!accessing!private! !
preCarga!public! !
reporteLiquidaciones!public! !
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

info
	|header|
	header:= 'Nombre: ', nombre, ' ', apellido, ' Obra Social: ', nombreObra.
^header!

montoCobertura
	^montoCobertura!

montoCobertura: anObject
	montoCobertura := anObject!

nombreObra
	^nombreObra!

nombreObra: anObject
	nombreObra := anObject!

operar: anOperacion
super operar:anOperacion.!

reporteLiquidaciones!

traerDatos! !
!PacienteConObra categoriesForMethods!
cargaDatos!public! !
incializa!public! !
info!public! !
montoCobertura!accessing!private! !
montoCobertura:!accessing!private! !
nombreObra!accessing!private! !
nombreObra:!accessing!private! !
operar:!public! !
reporteLiquidaciones!public! !
traerDatos!public! !
!

"Binary Globals"!

