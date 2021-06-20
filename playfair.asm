INCLUDE Irvine32.inc
BUFSIZE =5000
	.data
;messages appear for user
FileSize DWORD 0
E_OR_D Byte 'Enter The Type Of Operation (E/D) : ',0
C_OR_F Byte 'Do You Want To Read The Message From A File ? (Y/N) : ',0
Path   Byte 'Enter The Path Of The File : ',0
Message_From_Concole byte 'Enter your Message:  ',0
;Key    Byte ' Enter The Key : ',0
Technique  Byte 'Choose The  Technique For PlayFair Enter 1 Or Enter 2 For (1/2) : ',0
De_IN_File Byte 'Your Message has been Decrypted Successfully and Saved At This Path : ',0
En_IN_File Byte 'Your Message has been Encrypted Successfully and Saved At This Path : ',0
Txt_Msg    byte '.txt',0
En_Message Byte 'Your Encrypted Message is : ',0
De_Message Byte 'Your Decrypted Message is : ',0
Try_Again Byte 'Wrong Choise.. Please Try To Choose Again. ',0
Wrong_Input Byte 'Wrong Input..Please Try To Input Again. ',0
char1 DWORD 20 Dup(?) ,0
char2 byte 20 Dup(?),0
User_Key  Byte ?
CheckIJ byte 0
jf byte ?
flage dword 0
NewKeyC dword 0
conter dword 0
Error_Msg byte 'Error !!..',0
OutputFileName byte 1000 dup(?)
Counter_Length dword 0
Final_Key Byte 1000 DUP (?)
Counter   DWORD (1)
  WordBeforeModification byte 1000 DUP (?)
  WordAfterModification  byte 1000 DUP (?)
  WordWithoutRepition    byte 1000 DUP (?)

  UpdatedMsg             BYTE 1000 DUP (?)
  Input_Key            BYTE 'Enter The Key : ',0
New_Key              BYTE 'New Key IS    : ' ,0
MAT                  BYTE 'Matrix IS     : ',0
Key                  BYTE 1000 DUP (?)
KeyD                 BYTE 1000 DUP (?)   
Updated_Key          BYTE 1000 DUP (?)
;Counter              DWORD (0)
Char                 BYTE (65)                     ; Begin With 'A'
Equal                DWORD(1)
Temp                 DWORD ?
Table                BYTE  25 DUP (?)
Existing_Char        DWORD (0)
Ignor_J              BYTE  (0)
Ignor_I              BYTE  (0)
Put_I                BYTE  (0)
Message              BYTE 1000 dup (?)
Decer                BYTE 'Decrypted message is : ',0
Encer                BYTE 'ENcrypted message is : ',0 
Encrypted_Message    BYTE 1000 DUP (?)

Decrypted_Message     BYTE 1000 DUP (?)
Number_Of_Columns    DWORD (5)
Number_Of_Rows       DWORD (5)
First_Letter         DWORD (0)
Secound_Letter       DWORD (0)
First_ROW_NUMBER     DWORD   ? 
Secound_ROW_NUMBER   DWORD   ?
First_Bigger         DWORD   ?
Secound_Bigger       DWORD   ? 
First_V              BYTE   ?
Encrypt_Decrypt_Check BYTE 0
WrongKey byte 'You have enterd a wrong key',0
 checkk byte 0
 ;data for using files
    buffer BYTE BUFSIZE DUP(?)
	fileName BYTE 1000 DUP (?)
	FileHandle HANDLE ?					
	stringLength DWORD ?
	BytesWritten DWORD ?
	choice1_Var BYTE ?
	Error_With_File byte "Cannot Open The File!!..Please Try again. ",0
	Error_With_ReadingTheFile byte "Error With Reading The File.  ",0
	Error_With_BufferSize byte "Error: Buffer too small for file!!. ",0
	En byte '-Encrypted'
	De byte '-Decrypted'
	SizeE DWORD 0
	SizeD DWORD 0
	correctKey byte 1000 dup(?)
;String1 byte 'QUUGHBDNJJIQK'
String2 byte 1000 dup(?)
cnt byte 0
LEN DWORD 0	
.code
main proc

call ReadFromUser

exit
main endp
Check_Key PROC
mov ESI,offset Key
mov ECX,eax 
L1:
mov BL,[ESI]
cmp BL,73
je L2
mov BL,[ESI]
cmp BL,74
je L2
jmp next
L2: inc cnt
next:
inc esi
Loop L1
cmp cnt,2
je L3
jmp nth
L3:
Mov edx,offset WrongKey
call writestring
nth:
RET
Check_Key ENDP



;----------------------------------------------------------------------------------------------------------------------------
;First Function That Reads From user The Type of Operation that He/she want to make 
;Recieves : 
;    
;--------------------------------------------------------------------------------------------------------------------------------

ReadFromUser PROC USES  ECX

mov edx , offset E_OR_D    ;Message ask the user to enter the operation he/she want to make
call Display1              ;To display the message 

MOV EDX,OFFSET char1       ;EDX points to char1
MOV ECX , LENGTHOF char1   ;ECX Contains the max size 
call InputString           ;To take the Choice char from the user

call Check_Char1           ;Call the Function To check the char and know which path The program should take
RET
ReadFromUser ENDP

;-----------------------------------------------------------------------------------------------------------------------------------
;Second Function that Check the Choice of the user if valid or not if it's valid will check if it Encryption or Decryption Operation 
;if it's not valid will ask the user to enter the choice again
;Recieves : 
;    
;--------------------------------------------------------------------------------------------------------------------------------


Check_Char1 PROC 
Compare_D:         ;Label to compare if the Char was D/d Or E/e To know the type of operation
mov al ,'D'        ;Put 'D' into Al
cmp [edx],al       ;Compare Al-->'D' with the value in EDX--> The First Char of the word or the only char that user Entered
je  Decryption     ;Go To Decryption Label iF the Char was 'D'
mov al ,'d'        ;Put 'd' into Al 
cmp [edx],al       ;Compare Al-->'d' with the value in EDX--> The First Char of the word or the only char that user Entered
je  Decryption    ;Go To Decryption Label iF the Char was 'd'
mov al ,'E'         ;Put 'E' into Al
cmp [edx],al       ;Compare Al-->'E' with the value in EDX--> The First Char of the word or the only char that user Entered
je  Encryption    ;Go To Encryption Label iF the Char was 'E'
mov al ,'e'         ;Put 'e' into Al
cmp [edx],al       ;Compare Al-->'e' with the value in EDX--> The First Char of the word or the only char that user Entered
je  Encryption    ;Go To Encryption Label iF the Char was 'e'

call Crlf      ;EndLine 
jmp next

Encryption :          ;Call Function Encryption From that Label
call Encryption
jmp next
 
Decryption :          ;Call Function Decryption From that Label
call Decryption
jmp next

next :
ret
Check_Char1 ENDP    

;-----------------------------------------------------------------------------------------------------------------------------------
;If User Entered E for Encryption operation 
;Recieves : 
;    
;--------------------------------------------------------------------------------------------------------------------------------

Encryption PROC
MOV Encrypt_Decrypt_Check,1
l7:
mov CheckIJ,0
mov ebx,0
mov cnt,0
MOV EDX ,OFFSET C_OR_F  ;Message ask the user to enter if he want to read the word from file or Console/form
call Display1           ;To display the message

mov edx , offset char2
mov ecx , SizeOf char2
call InputString

call check_char2

MOV ESI,OFFSET buffer                 
MOV EDI ,OFFSET UpdatedMsg
mov ECX,EAX             ;The Real length 
call RemoveSpaces       ;If there is spaces between the words this function will remove it

mov EDX,offset UpdatedMsg
mov ECX,EbX
call CapitalAllLetters  ;If there is small letters this function will make it capital


mov EDX,offset UpdatedMsg
mov ECX,EbX
CALL Check_Capital_chars_Only
call crlf
cmp checkk,0
jne L7

MOV ESI , OFFSET UpdatedMsg
MOV EDX , OFFSET String2
MOV EAX , EBX
CALL editstring
MOV LEN,EBX
MOV EDX , OFFSET String2
CALL WRITESTRING

CALL Crlf
LoadAgain:
MOV EDX ,OFFSET Input_Key                
CALL Display1 
MOV EDX , OFFSET Key
MOV ECX , LENGTHOF Key
MOV ESI ,OFFSET Key 
CALL InputString  


MOV EDX,OFFSET Key                 
MOV EDI ,OFFSET Updated_Key
mov ECX,EAX  
call RemoveSpaces

MOV EDX,OFFSET Key                 
MOV EDI ,OFFSET Updated_Key
CALL Key_Modification        ; Modify Key 

mov EDX,offset Updated_Key
mov ECX,EAX
call CapitalAllLetters

mov esi,offset Updated_Key
mov ecx,ebx
call Check_KeyFrom_IJ
cmp CheckIJ,0
jne change

change:
mov esi,offset Updated_Key
mov edi,offset correctKey
mov ecx,ebx
Load:
L2:
mov al,[esi]
mov jf ,al
cmp al,74
je cont
cmp al,73
je cont
mov [edi],al
inc edi
inc NewKeyC
jmp sk
cont:
cmp flage , 0
je ya
jmp sk
ya:
mov al , jf
mov [edi] , al
inc edi
inc flage
inc NewKeyC
sk:
inc esi

Loop l2

mov esi,offset correctKey
mov edi,offset Updated_Key
mov ecx,NewKeyC
cld
rep movsb
MOV EDX,OFFSET Updated_Key
MOV ECX , NewKeyC
mov ebx,NewKeyC
call Display
call crlf
CALL  Key_Table 
CALL Display_Table
CALL CRLF

	   MOV ESI ,OFFSET String2
       MOV EDI ,OFFSET Encrypted_Message
	   MOV EAX,LEN
	   MOV Encrypt_Decrypt_Check ,1
	   CALL Encryption_Decryption
	
	MOV AL,'Y'
	   CMP [CHAR2],AL
	   JE WriteF
	    MOV AL,'y'
	   CMP [CHAR2],AL
	   JE WriteF	
	   MOV AL,'N'
	   CMP [CHAR2],AL
	   JE WriteCon
	    MOV AL,'n'
	   CMP [CHAR2],AL
	   JE WriteCon	

	   WriteF	:
	   CALL SaveInFileProcEn
	   mov edx,offset En_IN_File
	   call writestring
	   mov edx,offset OutputFileName
	   call writestring
	   
	   mov edx,offset TXT_Msg
	   call writestring
	   call crlf
	   jmp next
	   WriteCon     :
	   
	   
	   CALL Display_Encrypted
	   CALL CRLF

NEXT:	
ret
Encryption ENDP

Check_Char2 PROC 
Compare_Y:
mov al ,'Y'
cmp [edx],al
je  File
mov al,'y'
cmp [edx],al
je File

mov al ,'N'
cmp [edx],al
je  Console
mov al,'n'
cmp [edx],al
je  Console
call Crlf
mov edx , offset Try_Again
call WriteString 
call Crlf
call Crlf
POP ESI
call ReadFromUser
JMP NEXT

File:
call ReadFromF
jmp next
Console:
Call ReadFromConsole

next:
ret 
Check_Char2 ENDP

ReadFromConsole PROC
MOV EDX ,OFFSET Message_From_Concole
CALL Display1
MOV EDX,OFFSET buffer
MOV ECX ,LENGTHOF buffer
CALL InputString
RET
ReadFromConsole ENDP

ReadFromF PROC
mov edx,offset Path
call writestring
mov edx,offset fileName
mov ecx,lengthof fileName
call InputString
mov FileSize,eax
call Open_File
CMP EAX, INVALID_HANDLE_VALUE
JNE File_Opened
mov edx, offset Error_With_File
call writestring 
call Crlf
call Crlf
;call Open_File
;Jmp again
;again: call ReadFRomF
jmp next


File_Opened:
CALL Read_From_File
JNC Check_Buffer_Size
;JMP Close_File

Check_Buffer_Size:
CMP EAX, BUFSIZE
JB Buf_Size_Ok

Buf_Size_Ok:
;MOV buffer[eax], 0
;CALL WRITEDEC
;CALL CRLF

;MOV EDX, OFFSET buffer
;CALL WRITESTRING
;CALL CRLF

;Close_File:
;MOV EAX, FileHandle
;CALL CloseFile
next:
ret
ReadFromF ENDP

Decryption PROC
MOV Encrypt_Decrypt_Check,2
l7:
mov ebx,0
MOV EDX ,OFFSET C_OR_F  ;Message ask the user to enter if he want to read the word from file or Console/form
call Display1           ;To display the message

mov edx , offset char2
mov ecx , SizeOf char2
call InputString

call check_char2


MOV ESI,OFFSET buffer                 
MOV EDI ,OFFSET UpdatedMsg
mov ECX,EAX             ;The Real length 
call RemoveSpaces       ;If there is spaces between the words this function will remove it


mov EDX,offset UpdatedMsg
mov ECX,EbX
call CapitalAllLetters  ;If there is small letters this function will make it capital


mov EDX,offset UpdatedMsg
mov ECX,EbX
mov conter,ebx
CALL Check_Capital_chars_Only
call crlf
cmp checkk,0
jne L7

MOV EDX,OFFSET UpdatedMsg
MOV ECX , Ebx
call Display1
CALL Crlf
mov edx,0             ;if the word be with odd number of chars
mov eax,ebx
mov ebx,2
div ebx
cmp edx,0
JNE L10
JMP LoadAgain
L10:
MOV EBX,conter
dec EBX
MOV conter,EBX


LoadAgain:
MOV EDX ,OFFSET Input_Key                
CALL Display1 
MOV EDX , OFFSET Key
MOV ECX , LENGTHOF Key
MOV ESI ,OFFSET Key 
CALL InputString  


MOV EDX,OFFSET Key                 
MOV EDI ,OFFSET Updated_Key
mov ECX,EAX  
call RemoveSpaces

MOV EDX,OFFSET Key                 
MOV EDI ,OFFSET Updated_Key
CALL Key_Modification        ; Modify Key 

mov EDX,offset Updated_Key
mov ECX,EBX
call CapitalAllLetters
mov len,EBX
mov CheckIJ,0

mov esi,offset Updated_Key
mov ecx,ebx
call Check_KeyFrom_IJ
cmp CheckIJ,0
jne change

change:
mov esi,offset Updated_Key
mov edi,offset correctKey
mov ecx,ebx
Load:
L2:
mov al,[esi]
mov jf ,al
cmp al,74
je cont
cmp al,73
je cont
mov [edi],al
inc edi
inc NewKeyC
jmp sk
cont:
cmp flage , 0
je ya
jmp sk
ya:
mov al , jf
mov [edi] , al
inc edi
inc flage
inc NewKeyC
sk:
inc esi

Loop l2

mov esi,offset correctKey
mov edi,offset Updated_Key
mov ecx,NewKeyC
cld
rep movsb
MOV EDX,OFFSET Updated_Key
MOV ECX , NewKeyC
mov ebx,NewKeyC
call Display
call crlf
CALL  Key_Table 
CALL Display_Table
CALL CRLF

	   MOV ESI ,OFFSET UpdatedMsg
       MOV EDI ,OFFSET Decrypted_Message
	   MOV EAx,conter
	   MOV Encrypt_Decrypt_Check ,2
	   CALL Encryption_Decryption
	   MOV ESI , OFFSET Decrypted_Message
       MOV EDX , OFFSET String2
       call FiltrateString
	   MOV AL,'Y'
	   CMP [CHAR2],AL
	   JE WriteF
	    MOV AL,'y'
	   CMP [CHAR2],AL
	   JE WriteF	
	   MOV AL,'N'
	   CMP [CHAR2],AL
	   JE WriteCon
	    MOV AL,'n'
	   CMP [CHAR2],AL
	   JE WriteCon	

	   WriteF	:
	   CALL SaveInFileProcDe

	   mov edx,offset De_IN_File
	   call writestring

	   mov edx,offset OutputFileName     ;Message to Display the Path without .txt part
	   call writestring

	   mov edx,offset TXT_Msg            ;Message to Display ".txt" part
	   call writestring
	   call crlf
	   jmp next
	   WriteCon :
	   CALL Display_Decrypted
	   CALL CRLF
	   
next:
ret 
Decryption ENDP
InputString PROC USES EDX ECX    
call ReadString                                  ;Calling the function to let the user input the word.  
ret
InputString ENDP


RemoveSpaces PROC USES EDI ESI ECX
mov EBX,ECX
L1:

cmp byte PTR [esi] , 00100000b ;Check the value with space
je SpaceSkip                         ;If it's equal it will jmp to SpaceSkip
movsb
jmp next
cmp byte PTR [esi] ,00001010b

je SpaceSkip
movsb
jmp next

SpaceSkip:                            ;Skip this space and move to the next element in the word
inc esi
inc Counter_length
next:
Loop L1
Sub EBX,Counter_length
ret
RemoveSpaces ENDP

;-----------------------------------------------------------------------------------------------------------------------------------
;Take the letters and adding the bit number 5 with 0 so it will be always 0 that mean it will be capitl and save the another bits
;Receives:EDX Contains the offset of the word 
;		  ECX Contains the length of the word
;Returns: EDX
;-----------------------------------------------------------------------------------------------------------

CapitalAllLetters PROC USES ECX EDX  

L1:
AND byte PTR [edx], 11011111b ;to let the 5 bit be equal 0 so it's Capital and save the another bits by adding them with 1(1 and any value=that value)                       
inc edx
Loop L1

ret
CapitalAllLetters ENDP
Display PROC 


  MOV EDX ,OFFSET New_Key
  CALL WriteString 

                         
MOV ESI,OFFSET Updated_Key
MOV ECX , EBX
L5 :
MOV AL,[ESI]
CALL WriteChar
ADD ESI ,TYPE Updated_Key
LOOP L5
RET 
Display ENDP 

;-----------------------------------------------
;Displays a string to console
;Receives:EDX Contains the offset of the string  
;----------------------------------------------

Display1 PROC   Uses EDX                                  ;To test the result

CALL WriteString

ret
Display1 ENDP

;------------------------------------------------------------------------------------------------------------------
; Modify Key Which is Entered By the User By dropping any duplicate letters And the Spaces 
; Recieves :ESI Contains The OFFSET Of Key                 
;           EDI Contains The OFFSET Of Updated_Key
;           EAX Contains Length Of the Key
; Returns : EBX Contains Length Of the Updated_Key (KEY After Modification )
;-------------------------------------------------------------------------------------------------------------------

Key_Modification PROC 
mov counter,0
MOV ECX ,EAX                                  
L1 :
MOV Temp , ECX                                                           ; To save Value of ECX 

MOV EBX ,' '                                                             ; To Check Space 
MOV AL ,[ESI]
CMP EBX,EAX
JE Space

CMP Counter ,0                                                          ; To Check If it's First Letter 
JE Update                                                               ; If First Letter then Fill Updated_Key With it Directely Without Check previous 
                   

JMP Check                                                               ; Else Check If it's equal to One of the Letters  in the Updated_Key (dropping any duplicate letters )


Check :                                                
     MOV EDX , OFFSET Updated_Key
     MOV ECX,Counter
	 MOV Equal,1
    L2 :
    MOV AL ,[ESI]
    MOV BL ,[EDX]
    CMP AL,BL                                                           ; Compare With Previous Letters 
    JE Ignore
	MOV EquaL,0
    ADD EDX,TYPE Updated_Key
    LOOP L2


	CMP Equal ,0 
	JE Update

Update : 
    MOV AL,[ESI]                                                        ; Fill Updated_Key With Unduplicate letter 
    MOV [EDI],EAX
    INC Counter                                                         ; To Calculate  Length Of Updated_Key
	JMP Continue

Ignore :                                                            ; dropping any duplicate letters  
	 ADD ESI , TYPE Key
	 MOV ECX , Temp
	 JMP Next
Continue :                                                              ; Jmp ESI & EDI And return Value of ECX To L1 
    ADD ESI , TYPE Key
    ADD EDI , TYPE Updated_Key
	MOV ECX , Temp
	JMP Next
Space :                                                                 ; dropping any Spaces 
      ADD ESI , TYPE Key

Next :

LOOP L1 
MOV EBX ,Counter                                                        ; Get Length Of Updated_Key

RET
Key_Modification ENDP


EditString PROC 
;-----------------------------------------------------------------------------------------------------
; Function: Edits the KYE String by adding 'Q'.
; Resives :ESI = OFFSET of String1 ,
;EDX = OFFSET of String2 "NEW STRING" ,
;EAX = Lenghth of String
; Returns : New String , EBX = Lenghth of String2 , EDX =  OFFSET of String2
;-----------------------------------------------------------------------------------------------------
	PUSH EDX
	MOV EBX , 0
	MOV EDI , ESI
	INC EDI
	MOV ECX , EAX
	cmp eax,1
	je Done
	DEC ECX
	Outer:
		MOV AL ,BYTE PTR [ESI]
		CMP AL ,BYTE PTR [EDI]
		JE AddChar
		MOV BYTE PTR[EDX] , AL
		INC EDX
		INC EBX
		JMP Skip
			AddChar:
				MOV BYTE PTR[EDX] , AL
				INC EDX
				MOV EAX ,'Q'
				MOV BYTE PTR[EDX] , AL
				INC EDX
				ADD EBX , 2
			Skip:
				INC EDI 
				INC ESI
	LOOP Outer
	MOV AL , BYTE PTR [ESI]
	MOV [EDX] , AL
	INC EBX


;to check lenghth of string2 if it is odd

	MOV EAX , EBX
	MOV EDX , 0
	MOV ESI , 2
	DIV ESI
	CMP EDX , 0
	JNE IsOdd
	JMP Next
	IsOdd:
		POP EDX
		PUSH EDX
		ADD EDX , EBX
		MOV EAX , 0
		MOV EAX ,'Q'
		MOV [EDX] , AL
		INC EBX
		JMP Next
		done:
		
		POP EDX
		PUSH EDX
		MOV AL ,BYTE PTR [ESI]
		MOV BYTE PTR[EDX] , AL
		INC EBX
				INC EDX
				MOV EAX ,'Q'
				MOV BYTE PTR[EDX] , AL
				INC EBX
				JMP Next
	Next:
		POP EDX

	RET
EditString ENDP


FiltrateString PROC
;-----------------------------------------------------------------------------------------------------
; Function: Filtrates the  String by removeing Q .
; Resives :ESI = OFFSET of  ,
;EDX = OFFSET of String3 "NEW STRING" ,
;EAX = Lenghth of String2
; Returns : New String "String2" , EBX = Lenghth of String2 , EDX =  OFFSET of String2
;-----------------------------------------------------------------------------------------------------

	PUSH EDX
	PUSH ESI
	;ADD ESI , EAX
	;DEC ESI
	;MOV BL , BYTE PTR [ESI]
	;CMP BL ,'Q'
	;JE Remove
	JMP Lable1
	;Remove:
	;	DEC EAX 
	;	MOV BL , ' '
	;	MOV [ESI] , BL
	Lable1:
		POP ESI 
	MOV EDI , ESI
	ADD EDI , 2
	MOV ECX , EAX
	MOV EBX , 0
	SUB ECX , 2
	CMP ECX , 2
	JB L8
	Start:
		MOV AL ,BYTE PTR [ESI]
		CMP AL ,BYTE PTR [EDI]
		JE RemoveChar
		JMP Lable2
		RemoveChar:
		    INC ESI
			MOV AL ,BYTE PTR [ESI]
			CMP AL , 'Q'
			JE DELETE
			DEC ESI
			JMP Lable2
			DELETE:
			DEC conter
			DEC EAX
			DEC ECX
			DEC ESI
			MOV AL ,BYTE PTR [ESI]
			MOV [EDX] , AL
			INC EBX
			INC EDX
			ADD ESI , 2
			ADD EDI , 2
			JMP Lable2 
		Lable2:
			MOV AL ,BYTE PTR [ESI]
			MOV [EDX] , AL
			INC EBX
			INC EDX
			INC ESI
			INC EDI
	LOOP Start
	MOV EAX,conter
	L8:
	MOV ECX , 2
	L5:
	MOV BL , BYTE PTR [ESI]
	CMP BL ,'Q'
	je L6
	MOV BL ,BYTE PTR [ESI]
	CMP BL ,'Z'
	je L6
	MOV AL ,BYTE PTR [ESI]
	CMP AL,10d
	JNE L
	JMP E
	L: MOV [EDX],AL
	INC EBX
	INC ESI
	INC EDX
	JMP N
	L6:
	
		DEC EAX 
		MOV BL , ' '
		MOV [ESI] , BL
		N:
	LOOP L5 
	E:
	
	POP EDX
	RET
FiltrateString ENDP



;-----------------------------------------------------------------------------------------------------
;AL = Char entered from the user
;-----------------------------------------------------------------------------------------------------
Choice_Input Proc
MOV EAX, 0
CALL READCHAR
ret
Choice_Input ENDP

;-----------------------------------------------------------------------------------------------------
;Function: Creates a text File.
;EDX = OFFSET of FileName
;EAX contains a Valid File Handle if the Operation was Successful, otherwise
;EAX contain Invalid File Handle
;-----------------------------------------------------------------------------------------------------
Create_File PROC
MOV EDX, OFFSET fileName
CALL CreateOutputFile
MOV FileHandle, EAX
ret
Create_File ENDP

;-----------------------------------------------------------------------------------------------------
;Function: Opens a text File.
;EDX = OFFSET of FileName
;EAX contains a Valid File Handle if the Operation was Successful, otherwise
;EAX contain Invalid File Handle
;-----------------------------------------------------------------------------------------------------
Open_File Proc
MOV EDX, OFFSET fileName
CALL OpenInputFile
MOV FileHandle, EAX
ret
Open_File ENDP

;-----------------------------------------------------------------------------------------------------
;Function: Takes a text in array of char form from the user
;ECX = Buffer Size
;EDX = OFFSET of Buffer Array
;EAX = Length of the Text
;-----------------------------------------------------------------------------------------------------
Input_String PROC
MOV ECX, BUFSIZE
MOV EDX, OFFSET buffer
CALL READSTRING
MOV stringLength, eax
ret
Input_String ENDP

;-----------------------------------------------------------------------------------------------------
;Function: Write the Buffer Array taken from the user in the Text File
;EAX = File Handle
;EDX = OFFSET of the Buffer Array
;ECX = Length of the Array
;-----------------------------------------------------------------------------------------------------
Write_To_File PROC
MOV EAX, FileHandle
MOV EDX, OFFSET buffer
MOV ECX, stringLength
CALL WriteToFile
MOV BytesWritten, EAX
CALL CloseFile
ret
Write_To_File ENDP

;-----------------------------------------------------------------------------------------------------
;Function: Read From a Text File and passes what read to Buffer Array
;EDX = OFFSET of Buffer Array
;ECX = Buffer Size
;-----------------------------------------------------------------------------------------------------
Read_From_File PROC
MOV EDX, OFFSET buffer
MOV ECX, BUFSIZE
CALL ReadFromFile
ret
Read_From_File ENDP
Check_KeyFrom_IJ Proc
mov CheckIJ,0
MOV cnt,0
L1:
cmp cnt,2
je ErrorM
mov al,[esi]
inc esi
cmp al,73
je next 
jmp JCheck
JCheck:
cmp al,74
je next
jmp quit
next:
inc cnt
cmp cnt,2
je ErrorM
jmp quit
ErrorM:
Mov CheckIJ,1
call writestring
CALL crlf
jmp done

quit:
loop L1
done:
ret
Check_KeyFrom_IJ ENDP

Check_Capital_Chars_Only PROC
mov checkk ,0
L1:


mov al,[edx]
cmp al,64
jbe next 
jmp Check


Check:
cmp al,91
jae next
jmp quit


next:
mov edx,offset Wrong_Input
call writestring
jmp cont 
	
quit:
cmp ecx,1
je BB
BB:inc edx

loop L1
jmp ENDC
cont:
inc checkk
mov edx,0
;Call Encryption
ENDC:
ret
Check_Capital_Chars_Only ENDP
Mov CheckIJ,0


SaveInFileProcEn Proc USES EDI ESI ECX EAX
;--------------------------------------------------------
; Save result in file 
; Receives:nothing
; Returns: nothing 
;-------------------------------------------------------
cld
mov esi,offset fileName
mov edi,offset OutputFileName
mov ecx,FileSize
sub ecx,4
rep movsb
mov ecx,10
mov edx,offset En
L1:
mov al,[edx]
mov [edi],al
inc edx
inc edi
loop l1
mov edx,OFFSET OutputFileName ;Result.txt
call CreateOutputFile
mov fileHandle,eax
mov edx,OFFSET Encrypted_Message
mov ecx,SizeE-1
dec ecx
call WriteToFile
mov eax,fileHandle
call CloseFile
ret
SaveInFileProcEn Endp






SaveInFileProcDe Proc USES EDI ESI ECX EAX
;--------------------------------------------------------
; Save result in file 
; Receives:nothing
; Returns: nothing 
;-------------------------------------------------------
cld
mov esi,offset fileName
mov edi,offset OutputFileName
mov ecx,FileSize
sub ecx,4
rep movsb
mov ecx,10
mov edx,offset De
L1:
mov al,[edx]
mov [edi],al
inc edx
inc edi
loop l1
mov edx,OFFSET OutputFileName ;Result.txt
call CreateOutputFile
mov fileHandle,eax
mov edx,OFFSET Decrypted_Message
mov ecx,Lengthof Decrypted_Message
call WriteToFile
mov eax,fileHandle
call CloseFile
ret
SaveInFileProcDe Endp

;--------------------------------------------------------------------------------------------------------------------------------
;Generate the key table (Fill Matrix With Letters )
;Recieves : EBX Contains Length Of the Updated_Key (KEY After Modification )
;           ESI Contains OFFSET Of Updated_Key
;--------------------------------------------------------------------------------------------------------------------------------
Key_Table PROC
          
CLD  
                                            ;Copy Updated_Key In Matrix [ESI]->[EDI]
MOV ECX ,EBX 

MOV ESI,OFFSET correctkey
MOV EDI,OFFSET Table
REP MOVSB

MOV ESI,EDI                                      ;Make ESI Points TO First Empty Cell In Matrix 


MOV Existing_Char ,EBX                           ;TO Know How Many Filled Cell In matrix 
MOV Counter ,25                                  ;To Know How Many Cells Need TO Fill In Matrix 
SUB Counter ,EBX 
CMP Counter ,0
JE Already_Full
Check_I :                                        ;Check IF There Is (I)in The String 
                MOV EDI ,OFFSET Table
                MOV ECX,EBX
                PUSH ECX
                MOV AL ,73
			    MOV BX ,[EDI]
			    REPNE SCASB
				POP ECX
			    JE I_Exist 
			    JMP I_Not_Exist 
				
I_Exist :                                       ;If there is (I) then When Found J Ignore It & Start To Fill Matrix 
       MOV Ignor_J, 1 
       JMP Start
I_Not_Exist :                                  ; ELSE  Check IF There Is (J)in The String 
        JMP Check_J
        
Check_J :
                MOV EDI ,OFFSET Table         
                MOV AL ,74
			    MOV BX ,[EDI]
			    REPNE SCASB
			    JE J_Exist 
			    JMP J_Not_Exist 

J_Exist :
        MOV Ignor_I, 1                    ;If there is (j) then When Found I Ignore It & Start To Fill Matrix 
		JMP Start
J_Not_Exist :                             ; ELSE  Make it allowed To Put I 
        Mov Put_I   , 1

Start : 

MOV ECX , Counter
Fill_Table :                               ; First LOOP (LOOP  TO FILL Empty Cells)
      PUSH ECX                             ; To Save Value OF ECX 


	  MOV ECX,Existing_Char                ;Secound LOOP (LOOP TO Compare New Char With Already Existed Cells To Avoid any duplicate letters)
 Scan_Table :  
                               
              MOV EDX,ECX                  ; To Save Value OF ECX 

  MOV EDI ,OFFSET Table                    ;Check If The letter is Found Priviously OR not 
  MOV ECX,25 
 CHECK :
              
			  MOV AL ,Char
			  MOV BX ,[EDI]
			  REPNE SCASB
			  JE Found 
			  JMP Not_Found  

Found:                                     ;IF found Then Skip & Check The next 
	JMP SKIP 

Not_Found:                                 ;IF Not Found Check Before Put it in Matrix If It IS (I) OR (J)
    CMP AL ,73                             
	JE  J                                  ;IF I ->> Check IF J EXIST 
	CMP AL ,74                             ;IF J ->> Check IF I EXIST  
	JE  I
	JMP Accept	

J : 	 
	CMP Ignor_I,1                          ;IF J EXIST then Skip & Don't fill With it 
	JE SKIP
    JMP Fill_WithI                            ;Else Fill Matrix 
I :
    CMP Ignor_J ,1                         ;IF I EXIST then Skip & Don't fill With it 
	JE SKIP
    JMP Accept 
	                           ;Else Fill Matrix 
Fill_WithI :
MOV Ignor_J ,1
Accept:
    
    MOV [ESI],AL                           ;Put Char In Empty Cell & Jump To Next Empty 
	MOV BX ,[ESI]
	ADD ESI ,TYPE Table
	JMP NEXT


			  LOOP CHECK


	SKIP : 

			  MOV ECX,EDX                  ;Restore EDX
			  INC Char                      ; Check Next Char

 LOOP Scan_Table

 NEXT :
	 INC Existing_Char                     ;To Fill Another Letter
	  POP ECX                              ;Restore EDX

LOOP Fill_Table
Already_Full :

RET
Key_Table ENDP 
;----------------------------------------------------------------------------------------------------------------

Display_Table PROC 

  MOV EDX ,OFFSET MAT
  CALL WriteString 

                         
MOV ESI,OFFSET Table
MOV ECX , 25
L6 :
MOV AL,[ESI]
CALL WriteChar
ADD ESI ,TYPE Table
LOOP L6
RET 
Display_Table ENDP 
;------------------------------------------------------------------------------------------------
;Get Index Of Letters In Message To Be Encrypted 
;Recieves :ESI Contains OFFSET of  Message
;          EDI Contains OFFSET of  Encrypted_Message
;Returns  :EBX Contains Index Of First Letter
;          EAX Contains Index Of Secound Letter
;-------------------------------------------------------------------------------------------------
Get_Index_OF_Letters PROC USES ECX EDI EDX
       INC EAX
       MOV ECX,2  
	    


Index :    
              PUSH ECX 

			  MOV EDI ,OFFSET Table
              MOV ECX, lengthof Table
              MOV AL ,[ESI]
			  REPNE SCASB
			  JNE NOT_FOUND
              MOV EAX, LengthOf Table - 1
              SUB EAX, ECX

			  POP ECX
			  CMP ECX,1
			  JE Break     
			  JMP  Next
 NOT_FOUND :
   CMP Ignor_I ,1 
   MOV EDX,74
	   JE J_I  
   CMP Ignor_J,1 
   MOV EDX,73
      JE  J_I 

J_I :
             MOV EDI ,OFFSET Table
              MOV ECX, lengthof Table
              MOV AL ,DL
			  REPNE SCASB
			  JNE NOT_FOUND
              MOV EAX, LengthOf Table - 1
              SUB EAX, ECX
			  POP ECX
			  CMP ECX,1
			  JE Break     
			  JMP  Next
Next: 
      MOV First_Letter ,EAX
      ADD ESI,TYPE Message



  LOOP  Index

Break :
 MOV Secound_Letter ,EAX 
 ADD ESI ,TYPE Message

MOV EBX ,First_Letter

RET
Get_Index_OF_Letters ENDP
;------------------------------------------------------------------------------------------------------------
;Get Number Of Row Of Two Letters 
;Recieves : EBX Contains Index Of First Letter
;           EAX Contains Index Of Secound Letter
;Returns  : EBX Contains Number Of Row Of Secound Letter
;           EAX Conatins Number Of Row Of First   Letter 
;------------------------------------------------------------------------------------------------------------
GetRows_OF_Letters PROC USES  ECX 

 MOV EBX ,First_Letter
 MOV ECX ,2 
GetRows :
         CMP EBX,4
         JBE L1 
         CMP EBX,9
         JBE L2 
         CMP EBX,14
         JBE L3
         CMP EBX,19
         JBE L4
         CMP EBX,24
         JBE L5
		 JMP Conti
 L1 :
 MOV EBX ,0
 JMP Conti
 L2 :
 MOV EBX ,1
 JMP Conti
 L3 :
 MOV EBX ,2
 JMP Conti
 L4 :
 MOV EBX ,3
 JMP Conti
 L5 :
 MOV EBX ,4

 Conti :
                  CMP ECX ,2
				  JE  FIRST
				  JMP SECOUND
FIRST :
   MOV First_ROW_NUMBER ,EBX
   MOV EBX ,Secound_Letter
   JMP NEXT 
SECOUND :
 MOV Secound_ROW_NUMBER ,EBX
 NEXT :

 LOOP GetRows

 MOV EAX ,First_ROW_NUMBER

RET
GetRows_OF_Letters ENDP
;--------------------------------------------------------------------------------------------------------------
;Generate Encrypted Message By Fill NewString
;Recieves : EDI Contains OFFSET Of Encrypted_Message
;           EBX Contains If SameROW OR SameColumn OR NotSame 
;--------------------------------------------------------------------------------------------------------------
Generate_Encrypted_Message PROC USES ECX EAX ESI EBX 


CMP EBX ,1
JE ROW 
CMP EBX ,2 
JE COLUMN 
JMP Not_R_C

ROW :

MOV ECX ,2 
R1 : 

MOV ESI , OFFSET Table
CMP ECX,2 
JE  Row_1
JMP Row_2


Row_1 :

MOV EAX ,First_Letter
JMP CheckLast_Row

Row_2 : 
MOV EAX ,Secound_Letter
JMP CheckLast_Row


CheckLast_Row :
CMP EAX , 4
JE LAST_R 
CMP EAX,9
JE LAST_R 
CMP EAX,14
JE LAST_R
CMP EAX,19
JE LAST_R 
CMP EAX,24
JE LAST_R
JMP NOt_Last_R 
LAST_R :
SUB EAX,4
JMP Fill_Row

NOt_Last_R : 
INC EAX
JMP Fill_Row


Fill_Row: 
MOV EBX ,EAX
ADD ESI ,EBX
MOV EDX ,[ESI]
MOV [EDI],EDX
ADD EDI , TYPE Encrypted_Message

LOOP R1 
JMP Next 

COLUMN :


MOV ECX ,2 
CL1 : 

MOV ESI , OFFSET Table
CMP ECX,2 
JE  COL_1
JMP COL_2

COL_1 :
MOV EAX ,First_Letter
JMP CheckLast_Column 

COL_2 : 
MOV EAX ,Secound_Letter
JMP CheckLast_Column 

CheckLast_Column:
CMP EAX , 20
JE LAST_col 
CMP EAX,21
JE LAST_col 
CMP EAX,22
JE LAST_col
CMP EAX,23
JE LAST_col 
CMP EAX,24
JE LAST_col
JMP NOt_Last_col 

LAST_col :
SUB EAX,20
JMP Fill_Column

NOt_Last_col : 
ADD EAX ,5
JMP Fill_Column



Fill_Column : 
MOV EBX ,EAX
ADD ESI ,EBX
MOV EDX ,[ESI]
MOV [EDI],EDX
ADD EDI , TYPE Encrypted_Message

LOOP CL1
JMP Next


Not_R_C :
    MOV EBX , First_ROW_NUMBER 
    CMP EBX , Secound_ROW_NUMBER                          
	JB Exchange
	MOV First_Bigger ,1
    SUB EBX , Secound_ROW_NUMBER
    JMP Check_And_Fill

Exchange :
         MOV Secound_Bigger ,1
         MOV EBX , Secound_ROW_NUMBER                
		 SUB EBX , First_ROW_NUMBER


Check_And_Fill :
  
MOV ECX ,2 

Check : 
          PUSH ECX
                  CMP ECX ,2
				  JE  FIRST
				  MOV EDX,Secound_Bigger
				  MOV EAX ,Secound_Letter
				  JMP Up_OR_Down
First : 
MOV EDX,First_Bigger
MOV EAX ,First_Letter

Up_OR_Down :
            MOV ECX ,EBX 
		 L9:
               CMP EDX ,1
			  
			   JE UP

			   ADD EAX ,5
			   JMP BREAK 

UP : 
SUB EAX ,5
BREAK :
LOOP L9
Fill_NotSame : 
     MOV ESI , OFFSET Table
	 ADD ESI ,EAX 
	 POP ECX
	 CMP ECX ,2
	 JE L1 
	 MOV BL ,[ESI]
	 JMP CONT 
L1 :
	MOV AL ,[ESI]
   MOV First_V ,AL
CONT : 
	 LOOP Check
	 MOV AL ,First_V
MOV [EDI],BL
ADD EDI ,TYPE Encrypted_Message
MOV [EDI],AL
ADD EDI ,TYPE Encrypted_Message

Next :
MOV Secound_Bigger ,0
MOV First_Bigger ,0
RET
Generate_Encrypted_Message ENDP
;-----------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------
Generate_Decrypted_Message PROC USES ECX EAX ESI EBX 

CMP EBX ,1
JE ROW 
CMP EBX ,2 
JE COLUMN 
JMP Not_R_C

ROW :

MOV ECX ,2 
R1 : 

MOV ESI , OFFSET Table
CMP ECX,2 
JE  Row_1
JMP Row_2


Row_1 :

MOV EAX ,First_Letter
JMP CheckLast_Row

Row_2 : 
MOV EAX ,Secound_Letter
JMP CheckLast_Row


CheckLast_Row :
CMP EAX ,0
JE LAST_R 
CMP EAX,5
JE LAST_R 
CMP EAX,10
JE LAST_R
CMP EAX,15
JE LAST_R 
CMP EAX,20
JE LAST_R
JMP NOt_Last_R 
LAST_R :
ADD EAX,4
JMP Fill_Row

NOt_Last_R : 
DEC EAX
JMP Fill_Row


Fill_Row: 
MOV EBX ,EAX
ADD ESI ,EBX
MOV EDX ,[ESI]
MOV [EDI],EDX
ADD EDI , TYPE String2

LOOP R1 
JMP Next 

COLUMN :


MOV ECX ,2 
CL1 : 

MOV ESI , OFFSET Table
CMP ECX,2 
JE  COL_1
JMP COL_2

COL_1 :
MOV EAX ,First_Letter
JMP CheckLast_Column 

COL_2 : 
MOV EAX ,Secound_Letter
JMP CheckLast_Column 

CheckLast_Column:
CMP EAX , 0
JE LAST_col 
CMP EAX,1
JE LAST_col 
CMP EAX,2
JE LAST_col
CMP EAX,3
JE LAST_col 
CMP EAX,4
JE LAST_col
JMP NOt_Last_col 

LAST_col :
ADD EAX,20
JMP Fill_Column

NOt_Last_col : 
SUB EAX ,5
JMP Fill_Column



Fill_Column : 
MOV EBX ,EAX
ADD ESI ,EBX
MOV EDX ,[ESI]
MOV [EDI],EDX
ADD EDI , TYPE Encrypted_Message

LOOP CL1
JMP Next

Not_R_C :
    MOV EBX , First_ROW_NUMBER 
    CMP EBX , Secound_ROW_NUMBER                          
	JB Exchange
	MOV First_Bigger ,1
    SUB EBX , Secound_ROW_NUMBER
    JMP Check_And_Fill

Exchange :
         MOV Secound_Bigger ,1
         MOV EBX , Secound_ROW_NUMBER                
		 SUB EBX , First_ROW_NUMBER


Check_And_Fill :
  
MOV ECX ,2 

Check : 
          PUSH ECX
                  CMP ECX ,2
				  JE  FIRST
				  MOV EDX,Secound_Bigger
				  MOV EAX ,Secound_Letter
				  JMP Up_OR_Down
First : 
MOV EDX,First_Bigger
MOV EAX ,First_Letter

Up_OR_Down :
            MOV ECX ,EBX 
		 L9:
               CMP EDX ,1
			  
			   JE UP

			   ADD EAX ,5
			   JMP BREAK 

UP : 
SUB EAX ,5
BREAK :
LOOP L9
Fill_NotSame : 
     MOV ESI , OFFSET Table
	 ADD ESI ,EAX 
	 POP ECX
	 CMP ECX ,2
	 JE L1 
	 MOV BL ,[ESI]
	 JMP CONT 
L1 :
	MOV AL ,[ESI]
   MOV First_V ,AL
CONT : 
	 LOOP Check
	 MOV AL ,First_V
MOV [EDI],BL
ADD EDI ,TYPE Encrypted_Message
MOV [EDI],AL
ADD EDI ,TYPE Encrypted_Message

Next :
MOV Secound_Bigger ,0
MOV First_Bigger ,0



RET
Generate_Decrypted_Message ENDP
;---------------------------------------------------------------------------------------------------------------
;-----------------------------------------------------------------------------------------------------------------
Encryption_Decryption PROC USES EAX 

MOV EDX,0
MOV EBX ,2
DIV EBX

MOV ECX ,EAX                                       
Enc_Or_Dec :
      
	  CALL Get_Index_OF_Letters 
      MOV First_Letter,EBX
	  MOV Secound_Letter,EAX


      CALL  GetRows_OF_Letters
	 
    MOV EAX , First_Letter 
    CMP EAX , Secound_Letter                          ;2- Check Same Row OR Same Column OR Not Same Row&Column
	JB Exchange
    SUB EAX , Secound_Letter
    JMP Check_Position

Exchange :
         MOV EAX , Secound_Letter                
		 SUB EAX , First_Letter

Check_Position : 
               MOV EDX ,First_ROW_NUMBER
			   CMP EDX,Secound_ROW_NUMBER
			   JE Same_Row
               MOV EDX ,0 
			   MOV EBX ,5
			   DIV EBX 
			   CMP EDX ,0
			   JE Same_Column
			   JMP Not_Same
Same_Row : 
MOV EBX , 1
JMP NEXT
Same_Column :
MOV EBX , 2
JMP NEXT
Not_Same : 
MOV EBX , 3

NEXT : 
   CMP Encrypt_Decrypt_Check ,1
   JE Encrypt
   CALL Generate_Decrypted_Message
   JMP CON 
   Encrypt: 
  CALL Generate_Encrypted_Message

  CON :
LOOP Enc_Or_Dec 

RET
Encryption_Decryption ENDP
;-------------------------------------------------------------------------------
;-----------------------------------------------------------------------------
Display_Encrypted PROC

 MOV EDX ,OFFSET Encer
  CALL WriteString 

  MOV EDI ,OFFSET Encrypted_Message
  MOV ECX ,len
DIS :

MOV AL ,[EDI]
CALL WriteChar
ADD EDI , TYPE Encrypted_Message
LOOP DIS 


RET

Display_Encrypted ENDP
;---------------------------------------------------------------------------------
;---------------------------------------------------------------------------------
Display_Decrypted PROC


 MOV EDX ,OFFSET Decer
  CALL WriteString 

  MOV EDI ,OFFSET string2
  MOV ECX ,Ebx
  MOV SizeE,Ebx

DIS :

MOV AL ,[EDI]
CALL WriteChar
ADD EDI , TYPE Decrypted_Message
LOOP DIS 
RET
Display_Decrypted ENDP


end main
