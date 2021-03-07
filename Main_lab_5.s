; Archivo:	        Main_lab_5.s
; Dispositivo:	        PIC16F887
; Autor:	        Dylan Ixcayau
; Compilador:	        pic-as (v2.30), MPLABX V5.45
;                
; Programa:	        Displays simultaneos
; Hardware:	        LEDs en el puerto A, Display en el puerto C y botones en el puerto B
;                       
; Creado:                3 mar, 2021
; Última modificación:   3 mar, 2021

;---------------------------------------------------------------------------------------------
PROCESSOR 16F887
#include <xc.inc>

; configuración word1
 CONFIG FOSC=INTRC_NOCLKOUT //Oscilador interno sin salidas
 CONFIG WDTE=OFF	    //WDT disabled (reinicio repetitivo del pic)
 CONFIG PWRTE=ON	    //PWRT enabled (espera de 72ms al iniciar
 CONFIG MCLRE=OFF	    //pin MCLR se utiliza como I/O
 CONFIG CP=OFF		    //sin protección de código
 CONFIG CPD=OFF		    //sin protección de datos
 
 CONFIG BOREN=OFF	    //sin reinicio cuando el voltaje baja de 4v
 CONFIG IESO=OFF	    //Reinicio sin cambio de reloj de interno a externo
 CONFIG FCMEN=OFF	    //Cambio de reloj externo a interno en caso de falla
 CONFIG LVP=ON		    //Programación en bajo voltaje permitida
 
;configuración word2
  CONFIG WRT=OFF	//Protección de autoescritura 
  CONFIG BOR4V=BOR40V	//Reinicio abajo de 4V 
 
    
  PSECT udata_bank0 ;common memory
    var:	DS  1 ;1 byte apartado
    cont:	DS  1
    centenas:	DS  1
    cen:	DS  1
    dece:	DS  1
    uni:	DS  1
    decenas:	DS  1
    unidades:	DS  1
    banderas:	DS  1
    nibble:	DS  2 ;2 byte apartado
    display_var:    DS	2 ;2 byte apartado
    
  PSECT udata_shr ;common memory
    w_temp:	DS  1;1 byte apartado
    STATUS_TEMP:DS  1;1 byte
  
  PSECT resVect, class=CODE, abs, delta=2
  ;----------------------vector reset------------------------
  ORG 00h	;posición 000h para el reset
  resetVec:
    PAGESEL main
    goto main
    
  PSECT intVect, class=CODE, abs, delta=2
  ;----------------------Macros------------------------------------
  dispdiv macro   ;Activamos el macros 
    movf    centenas, w	 ;Muevo el valor de la variable de la centenas a w
    call    Tabla	 ;Mando w a la tabla 
    movwf   cen		 ;Meto w convertido a la variable cen
    movf    decenas, w
    call    Tabla
    movwf   dece
    movf    unidades, w
    call    Tabla
    movwf   uni
    endm
    
  PSECT code, delta=2, abs
  ORG 100h	;Posición para el código
 ;------------------ TABLA -----------------------
  Tabla:
    clrf  PCLATH
    bsf   PCLATH,0
    andlw 0x0F
    addwf PCL
    retlw 00111111B          ; 0
    retlw 00000110B          ; 1
    retlw 01011011B          ; 2
    retlw 01001111B          ; 3
    retlw 01100110B          ; 4
    retlw 01101101B          ; 5
    retlw 01111101B          ; 6
    retlw 00000111B          ; 7
    retlw 01111111B          ; 8
    retlw 01101111B          ; 9
    retlw 01110111B          ; A
    retlw 01111100B          ; b
    retlw 00111001B          ; C
    retlw 01011110B          ; d
    retlw 01111001B          ; E
    retlw 01110001B          ; F
 
  ;---------------configuración------------------------------
  main: 
    banksel ANSEL	    ;Llamo al banco de memoria donde estan los ANSEL
    clrf    ANSEL	    ;Pines digitales
    clrf    ANSELH
    
    banksel TRISA	    ;Llamo al banco de memoria donde estan los TRISA y WPUB
    clrf    TRISA	    ;Al usar todos los pines de este puerto dejo todos como salida digital
    clrf    TRISC	    ;Al usar todos los pines de este puerto dejo todos como salida digital
    clrf    TRISD
    
    bcf	    OPTION_REG, 7   ;Activo la opcion de las resistencias en el PUERTOB 
    bsf	    WPUB, 0	    ;Activo las resistencias de los puertos que usare
    bsf	    WPUB, 1
    
    bsf	    TRISB, 0	    ;Dejo como entradas los puertos que usare para los botones
    bsf	    TRISB, 1
    
    banksel PORTA	    ;Llamo al banco de memoria donde estan los PORT
    clrf    PORTA	    ;Limpio los puertos de salidas digitales
    clrf    PORTC
    clrf    PORTD
     	
    call    config_reloj    ;Configuracion de reloj para darle un valor al oscilador
    call    config_IO	    ;Configuracion de las interrupciones del Puerto B
    call    config_timr0    ;Configuracion del timer 0
    call    config_IE	    ;Configuracion de las interrupciones del timer0
    banksel PORTA
;----------loop principal---------------------
 loop:
    movf    PORTA, w	    ;Muevo el valor de PORTA a w
    movwf   var		    ;Muevo w a var
    movwf   cont	    ;Muevo w a cont
    
    call    separar_nibbles ;LLamo a la rutina para separar el valor en nibbles
    call    config_displays ;Llamo a la configuración de los displays
    call    Centenas	    ;Llamo a la rutina para la división
    dispdiv		    ;Macro
    
    goto    loop    ;loop forever 
 ;----------------------interripción reset------------------------
  ORG 04h	;posición 0004h para interr
push:			    
    movf    w_temp	    ;Guardamos w en una variable temporal
    swapf   STATUS, W	    ;Sustraemos el valor de status a w sin tocar las interrupciones
    movwf   STATUS_TEMP	    ;Guardamos el status que acabamos de guardar en una variable temporal
    
  isr:
    btfsc   T0IF	    ;Si el timer0  levanta ninguna bandera de interrupcion
    call    TMR0_interrupt  ;Rutina de interrupcion del timer0
    
    btfsc   RBIF	    ;Si el puerto B levanta la banderas de interrupcion
    call    IOCB_interrupt  ;Rutina de interrupcion del puerto B
    
  pop:
    swapf   STATUS_TEMP, W  ;Recuperamos el valor del status original
    movwf   STATUS	    ;Regresamos el valor a Status
    swapf   w_temp, F	    ;Guardamos el valor sin tocar las banderas a F
    swapf   w_temp, W	    ;El valor normal lo dejamos en w
    retfie		    ;Salimos de las interrupciones
;---------SubrutinasInterrupción-----------
IOCB_interrupt:
    banksel PORTB
    btfss   PORTB, 0	    ;Revisamos el primer boton
    incf    PORTA	    ;incrementamos el puerto A y por ende el display
    btfss   PORTB, 1	    ;Revisamos el segundo boton 
    decf    PORTA	    ;decrementamos el puerto A y por ende el display
    bcf	    RBIF	    ;Bajamos la bandera del puerto B
    return
TMR0_interrupt:
    call    timr0
    bcf	    STATUS, 0	    ;Dejo el STATUS 0 en un valor de 0
    clrf    PORTD	    ;Limpio el puerto D
    btfsc   banderas, 1	    ;Las banceras me ayudaran a hacer los saltos entre cada display
    goto    display0	    ;Si la variable vandera es 1 en la posicion 1 vamos a la rutina del display0
    btfsc   banderas, 2 
    goto    display1
    btfsc   banderas, 3 
    goto    display2	    ;Display de la centena
    btfsc   banderas, 4 
    goto    display3	    ;Dislay de la decena
    btfsc   banderas, 5 
    goto    display4	    ;Display de la unidad
    movlw   00000001B
    movwf   banderas
siguientedisplay:
    RLF	    banderas, 1	    ;Me ayuda a cambiar la bandera e ir al siguiente display
    return
    
display0:		    
    movf    display_var, w  ;La variable display tiene el valor que necesito ya modificado para hexadecimal
    movwf   PORTC	    ;Despues de pasar display_var a w movemos w al puertoC
    bsf	    PORTD, 0	    ;seteamos el pin del puerto D para controlar que display se mostrara
    goto    siguientedisplay
    
display1:
    movf    display_var+1, w
    movwf   PORTC
    bsf	    PORTD, 1
    goto    siguientedisplay
    
display2:
    movf    cen, w
    movwf   PORTC
    bsf	    PORTD, 2
    goto    siguientedisplay
    
display3:
    movf    dece, w
    movwf   PORTC
    bsf	    PORTD, 3
    goto    siguientedisplay
    
display4:
    movf    uni, w
    movwf   PORTC
    bsf	    PORTD, 4
    goto    siguientedisplay
;------------sub rutinas--------------------
separar_nibbles:
    movf    var, w	    ;Var tiene el valor del contador
    andlw   0x0f	    ;Obtenemos los 4 bits menos significativos
    movwf   nibble	    ;los pasamos a nibble
    swapf   var, w	    ;volteamos la variable var
    andlw   0x0f	    ;obtenemos los 4 bits mas significativos
    movwf   nibble+1	   ; Los pasamos a nibble+1
    return
    
    
Centenas:
    clrf    centenas	    ;limpiamos la variable que guardara las centenas
    movlw   1100100B	    ;Movemos 100 en binario a w
    subwf   cont, 1	    ;cont tiene el valor del contador y se le restan los 100 del paso anterior
    btfss   STATUS, 0	    ;Revisamos si la resta nos deja un carry indicando que el valor en negativo
    goto    Decenas	    ;si tenemos un carry nos pasamos a las decenas
    incf    centenas, 1	    ;Si aun no tenemos el carry iremos incrementando la variable centenas
    goto    $-5		    ;Hacemos el procedimiento otra vez
			    
Decenas:
    clrf    decenas
    movlw   01100100B	    
    addwf   cont	    ;Le sumamos los 100 anteriores para que el valor no sea negativo
    movlw   00001010B
    subwf   cont, 1
    btfss   STATUS, 0
    goto    Unidades
    incf    decenas, 1
    goto    $-5
    
Unidades:
    clrf    unidades
    movlw   00001010B	    ;Le sumamos los 10 anteriores para que el valor no sea negativo
    addwf   cont
    movlw   00000001B
    subwf   cont, 1
    btfss   STATUS, 0
    return
    incf    unidades, 1
    goto    $-5
    
config_displays:
    movf    nibble, w	    ;Movemos el valor de nibble a w
    call    Tabla	    ;Movemos w a la tabla
    movwf   display_var	    ;el valor de w preparado para el display lo madamos a la variable display_var
    movf    nibble+1, w	    ;
    call    Tabla
    movwf   display_var+1
    return
config_IO:
    banksel TRISA
    bsf	    IOCB, 0	;encendemos la interrupcion del puerto
    bsf	    IOCB, 1	;encendemos la interrupcion del puerto
    
    banksel PORTA
    movf    PORTB, W	;Condición mismatch
    bcf	    RBIF
    return
     
 config_timr0:
    banksel OPTION_REG	    ;Banco de registros asociadas al puerto A
    bcf	    T0CS	    ; reloj interno clock selection
    bcf	    PSA		    ;Prescaler 
    bsf	    PS2
    bsf	    PS1
    bsf	    PS0		   ;PS = 111 Tiempo en ejecutar , 256
    
    banksel TMR0
    call    timr0
    return
    
 config_reloj:
    banksel OSCCON	;Banco OSCCON 
    bsf	    IRCF2	;OSCCON configuración bit2 IRCF
    bsf	    IRCF1	;OSCCON configuracuón bit1 IRCF
    bcf	    IRCF0	;OSCCON configuración bit0 IRCF
    bsf	    SCS		;reloj interno , 4Mhz
    return

config_IE:
    bsf	    GIE		;Habilitar en general las interrupciones, Globales
    bsf	    RBIE	;Se encuentran en INTCON
    bcf	    RBIF	;Limpiamos bandera
    bsf	    T0IE
    bcf	    T0IF	;Limpiamos bandera
    return
 
timr0:
    banksel TMR0
    movlw   237	
    movwf   TMR0
    bcf	    T0IF	;5 ms
    return
end


