; Table of Procedure Offsets          (ProcTble.asm)

; This progam contains a table with offsets of procedures.
; It uses the table to execute indirect procedure calls.

INCLUDE Irvine32.inc



;PROTO Statements



HTCreate PROTO,
	hashSize:BYTE,
	hashName: PTR BYTE

HTInsert PROTO,
	htptr:DWORD,
	htkey:PTR BYTE,
	htvalue:PTR BYTE

HTRemove PROTO,
	htptr:DWORD,
	htkey:PTR BYTE

HTSearch PROTO,
	htptr:DWORD,
	htkey:PTR BYTE

HTPrint PROTO,
	htptr:DWORD

HTDestroy PROTO,
	htptr:DWORD,
	hthheap:HANDLE

.data

hHeap   HANDLE ?		; main handle to the process heap

hashPTR DWORD ? ; stores the address of the hashtable


msgcreate BYTE "1: HTCreate",0
msgasksize BYTE "What size?: ",0
msgKeyNotFound BYTE "Key not found. ",0
msgKeyFound BYTE "Key Found ",0
msgInsertingKey BYTE "Inserting Key...",0
msgValue BYTE "Value: ",0
msgNothing BYTE "Nothing Found. ",0
msgColon BYTE " : ",0 
msgTableName BYTE "Table Name: ",0
msgTableSize BYTE "Table Size: ",0
msgElementsInTable BYTE "Elements in Table: ",0
msgArrow BYTE " --> ",0
msgKeyExists BYTE "Key exists. ",0
msgNotFound BYTE "Not Found. ",0
msgKeyUpdated BYTE "Key Updated. ",0
msgKey BYTE "Key: ",0


avengersHashPTR DWORD ? ; stores the address of the avengers table
avengersHHeap HANDLE ? ; stores heap handle for avengers
avengersTableSize BYTE 5
msgAvengers BYTE "Avengers",0 ; use for table

badGuysHashPTR DWORD ? ; stores the address of the avengers table
badGuysHHeap HANDLE ? ; stores heap handle for avengers
badGuysTableSize BYTE 6
msgBadGuys BYTE "Bad Guys",0 ; use for table

msgThor BYTE "Thor",0
msgHemsworth BYTE "Hemsworth",0

msgIronman BYTE "Ironman",0
msgDowney BYTE "Downey",0

msgHulk BYTE "Hulk",0
msgRuffalo BYTE "Ruffalo",0

msgOdin BYTE "Odin",0

msgJarvis BYTE "Jarvis",0
msgBettany BYTE "Bettany",0

msgFury BYTE "Fury",0
msgJackson BYTE "Jackson",0

msgLoki BYTE "Loki",0
msgHiddleston BYTE "Hiddleston",0

msgUltron BYTE "Ultron",0
msgSpader BYTE "Spader",0


.code



main PROC

restart:
	;mov edx, OFFSET msgwelcome
	;call WriteString
	

avengersAssemble:
	

	INVOKE HTCreate, avengersTableSize, addr msgAvengers ;create a hashtable for avengers with size of 5
	;edx has the pointer to hashtable after calling htcreate
	;eax has the heap pointer after calling htcreate
	mov avengersHashPTR,edx
	mov avengersHHeap, eax
	INVOKE HTInsert, avengersHashPTR, addr msgThor, addr msgHemsworth
	INVOKE HTInsert, avengersHashPTR, addr msgIronman, addr msgDowney
	INVOKE HTInsert, avengersHashPTR, addr msgHulk, addr msgRuffalo
	INVOKE HTPrint, avengersHashPTR
	INVOKE HTSearch, avengersHashPTR, addr msgIronman
	call crlf
	mov edx,edi ; value address
	call WriteString ; print the value returned 
	call crlf

	call crlf

	INVOKE HTSearch, avengersHashPTR, addr msgThor
	call crlf
	mov edx,[edx] ; dereference to get address of value
	call WriteString ; print the value returned 
	call crlf

	INVOKE HTRemove, avengersHashPTR, addr msgThor
	INVOKE HTPrint, avengersHashPTR
	INVOKE HTRemove, avengersHashPTR, addr msgOdin
	INVOKE HTPrint, avengersHashPTR
	INVOKE HTSearch, avengersHashPTR, addr msgIronman
	call crlf
	mov edx,edi ; value address
	call WriteString ; print the value returned 
	call crlf
	INVOKE HTSearch, avengersHashPTR, addr msgThor
	call crlf
	mov edx,edi ; value address
	call WriteString ; print the value returned 
	call crlf
	INVOKE HTInsert, avengersHashPTR, addr msgThor, addr msgHemsworth
	INVOKE HTInsert, avengersHashPTR, addr msgJarvis, addr msgBettany
	INVOKE HTInsert, avengersHashPTR, addr msgFury, addr msgJackson
	INVOKE HTPrint, avengersHashPTR

	INVOKE HTCreate, badGuysTableSize, addr msgBadGuys ;create a hashtable for avengers with size of 5
	;edx has the pointer to hashtable after calling htcreate
	;eax has the heap pointer after calling htcreate
	mov badGuysHashPTR,edx
	mov badGuysHHeap, eax
	INVOKE HTInsert, badGuysHashPTR, addr msgLoki, addr msgHiddleston
	INVOKE HTInsert, badGuysHashPTR, addr msgUltron, addr msgSpader
	INVOKE HTPrint, badGuysHashPTR
	INVOKE HTPrint, avengersHashPTR
	
	INVOKE HTDestroy, avengersHashPTR, avengersHHeap ; does nothing, not implemented
	INVOKE HTPrint, avengersHashPTR

	INVOKE HTDestroy, badGuysHashPTR, badGuysHHeap ; does nothing, not implemented
	INVOKE HTPrint, badGuysHashPTR

	exit
main ENDP

;-------------------------------
HTCreate PROC uses ebx ecx esi,
	hashSize:BYTE, hashName:PTR BYTE
; Creates an empty hash table
; Receives: the hash size (to make a table of size(hashsize)) , byte pointer to string name for hash table
;Returns: pointer to the hash table in edx
;edx has the pointer to hashtable after executing
;eax has the heap pointer after executing

	INVOKE GetProcessHeap ; get handle to heap
	mov hHeap, eax ; hHeap stores the handle
	push eax ; save handle for later

	movzx eax, hashSize
	add eax,3
	;^ reserve 3 slots at the front for 1. the hash table name 2. the hash table size 3. the number of elemetns in the hash table 
	mov edx, SIZEOF DWORD
	mul edx
	; eax now has the correct memory space for hash table

	INVOKE HeapAlloc, hHeap, HEAP_ZERO_MEMORY, eax ; allocate space for hash table
	; eax has the pointer to the table

	mov edx,eax ; put pointer to table in edx for return
	mov edi,hashName
	mov [eax],edi ; address to name of hashtable in first slot now
	add eax,4
	movzx ebx, hashSize
	mov [eax],ebx ; hashsize stored in second slot
	; third slot is already 0 by default, which contains number of elements in the table
	pop eax

	;when we return, eax will hold the heap handle, and edx will hold pointer to the table
	ret
HTCreate ENDP

;------------------------------------
HTInsert PROC uses ebx ecx esi edx,
	htptr:DWORD, htkey:PTR BYTE, htvalue:PTR BYTE
; inserts a key- value pair into the hash table
; if insert will cause 75% load factor to be exceeded, double the hash size and recompute all hash keys <-- NOT IMPLEMENTED
	call crlf
	;check if key exists
	INVOKE HTSearch, htptr, htkey

	cmp eax, 1
	jne noExist

	; if the key does exist, we can update the value here
	mov ecx, htvalue
	mov [edx],ecx ; after HTSearch, edx will contain a pointer to the address of the value. Dereference it to change value
	mov edx, OFFSET msgKeyExists
	call WriteString

	
	mov edx, OFFSET msgKeyUpdated
	call WriteString
	call crlf
	jmp endInsert
	

	;safe to add key after this point
noExist:
	
	mov edx, offset msgInsertingKey ;
	call WriteString
	call crlf
	mov esi, htptr ; move the pointer of hash table into esi
	add esi,8 ; go to number of elements
	mov ecx,00000001
	add [esi],ecx ; increment number of elements in the table 
	
	mov esi, htptr ; reset to front of table

	; move address of string key into esi
	mov edx, htkey ; move address of the key into edx
	
    call StrLength

     ; length of the string in eax
	mov ecx, eax
	mov eax,00000000
	mov ebx,00000000
	
L1: 
	mov al, [edx] ; move a character into al
	add ebx, eax ; add the character ascii value to a counter
	inc edx ; next character
	loop L1

	; ebx now has full ascii value

	add esi,4 ; increment to hash table size
	mov edi, [esi]
	mov eax,ebx ; full ascii value is the divisor
	mov edx, 00000000 ; clear edx to make room for remainder
	div edi ; htptr+4 is the table size
	; remainder stored in edx
	add esi,8 ; go to first element in array

	mov eax,edx
	;eax contains how many slots to increment
	push ebx
	mov ebx, 00000004
	mul ebx ; multiply by dword
	pop ebx
	add esi,eax ; go to the spot in the table we should be at

	;esi contains the address where we should put the newly allocated heap

	; code to update value if key is present

	


proceed:

	INVOKE GetProcessHeap
	mov ebx, eax ; ebx has the heap pointer

	; we want to allocate 4 spaces for each node
	mov eax, 00000000
	add eax, 16 ; 4 * SIZEOF DWORD = 16
	
					;proc heap  in ebx        space of a node in eax
	INVOKE HeapAlloc, ebx, HEAP_ZERO_MEMORY, eax ; allocate space for a node

	; esi has a pointer to the slot we need to search through
	; ebx has heap handle
	; eax has pointer to the array we recently allocated
	
	push esi ; store slot for later
	mov edx, eax ; store address of newly created node in edx for later
	mov ecx, htkey
	mov [eax],ecx; key goes in first slot
	add eax,4
	mov ecx, htvalue
	mov [eax],ecx ; value goes in second slot
	
	add eax,8 ; skip to heap handle slot
	mov [eax],ebx ; put heap handle in last slot of node

	mov eax,edx ; restore original address of array to eax

	
	
	mov ebx,[esi] ; ebx contains the address of the node
	cmp ebx,0 ; if its 0, the slot is free
	jne notFree
	; past this point, the slot is free
	mov [esi], eax
    jmp endInsert


notFree:

	; eax stores the address of the succesfully allocated array
	; esi stores the address of the correctly hashed slot in the array
	; 
	mov edi, eax
	add edi,8 ; go to 3rd bucket, which is next link
	mov [edi], ebx ; store the address of next node in edi
	mov [esi],eax
	 
	; NODE STRUCTURE 
	; | KEY | VALUE | PTRNEXTNODE | HEAPHANDLE |


endInsert:
	
	ret

	
HTInsert ENDP

;------------------------------------
HTRemove PROC uses ebx ecx esi edx,
	htptr:DWORD, htkey:PTR BYTE
; removes a key-value pair from the hash table
; inputs - a pointer to the hash table, a key
; returns nothing

	INVOKE HTSearch, htptr, htkey

	cmp eax,0 ; if key no exist, we stop

	je done

	; used from HTInsert
	mov esi, htptr ; move the pointer of hash table into esi
	
	; move address of string key into esi
	mov edx, htkey ; move address of the key into edx
	


    call StrLength

    ; length of the string in eax
	mov ecx, eax
	mov eax,00000000
	mov ebx,00000000
	
L1: 
	mov al, [edx] ; move a character into al
	add ebx, eax ; add the character ascii value to a counter
	inc edx ; next character
	loop L1

	; ebx now has full ascii value

	add esi,4 ; increment to hash table size
	mov edi, [esi]
	mov eax,ebx ; full ascii value is the divisor
	mov edx, 00000000 ; clear edx to make room for remainder
	div edi ; htptr+4 is the table size
	; remainder stored in edx
	add esi,8 ; go to first element in array

	mov eax,edx
	;eax contains how many slots to increment
	push ebx
	mov ebx, 00000004
	mul ebx ; multiply by dword
	pop ebx
	add esi,eax ; go to the spot in the table we should be at
	;end of segment used from insert
	
	
	mov eax, htkey 
	
	; esi contains the slot in the hash table that we should look for the key
	; eax contains the key address

	mov ebx,[esi] ; ebx stores address of first node

	cmp ebx,0 ; if ebx = 0, nothing to remove
	je notFound


	INVOKE Str_compare, [ebx], eax ; test value of first node against key

	jne noDeleteAtHead

deleteAtHead:	; past this point, we found the node to be deleted
	
	; need to store address of next in node to be deleted where its address is

	; esi points to the correct slot in the table
	; deferencing esi points to the node
	
	mov edi, [esi] 
	mov ebx, esi ; ebx will be used to store the pointer to the memory block

	add edi, 8 ; increment 2 dwords in order to get pointer next node (even if its 0)
	mov eax,[edi]
	
	mov [esi],eax ; node is now lost-- the slot in the table now points to the lost node's nxt
	add edi,4 ;edi is now the heap handle

	INVOKE HeapFree, [edi], 0, [ebx] 
	mov esi, htptr ; move the pointer of hash table into esi
	add esi,8
	mov ecx,00000001
	sub [esi],ecx ; decrement number of elements in the table 
	jmp done

	
noDeleteAtHead:
repeatSearch:
	;ebx stores address of first node
	; edx will store prev node
	mov edx, ebx ; edx has first address in node
	add ebx, 8
	mov ebx, [ebx] ; go to next node, if it exists
	mov eax, ebx ; eax will store the current node

	cmp ebx, 0 ; check if the next node is blank
	je notFound ; if its blank, we go to next node

	; if not blank, we need to check this node
	push eax
	mov edi, [ebx] ; store address of key in edi
	mov eax, htkey ; address of key we looking to remove in eax
	INVOKE Str_compare, edi,eax
	pop eax

	jne notEqual

	mov edi, eax ; store the address of current node to remove in edi
	add edx,8 ; edx points to previous node's next
	add eax,8 ; eax points to current node's next
	 ;eax stores address of node we want to patch out
	mov esi, [eax] ; get next of current node, store in esi
	mov [edx],esi ; store next of current node in previous node's next 
	; remove node at address edi

	mov esi, edi
	add esi,12 ; increment esi to the heap handle
	INVOKE HeapFree, [esi], 0, edi 
	mov esi, htptr ; move the pointer of hash table into esi
	add esi,8
	mov ecx,00000001
	sub [esi],ecx ; decrement number of elements in the table 


	jmp done

notEqual:
	mov ebx, eax
	jmp repeatSearch

notFound:
	
	mov edx, offset msgNotFound
	call WriteString
	call crlf

done:
	ret
HTRemove ENDP
	

;------------------------------------
HTSearch PROC uses ebx ecx esi ,
	htptr:DWORD, htkey:PTR BYTE
; find a key in the hash table and return the value
; returns the value of the key if found
; eax contains a 1 if found, or a 0 if not found
; edx contains the address of the value, will be blank if address not found
; edi contains the value, blank if address not found
	
	call crlf
	; used from HTInsert
	mov esi, htptr ; move the pointer of hash table into esi
	
	; move address of string key into esi
	mov edx, htkey ; move address of the key into edx
	
    call StrLength

    ; length of the string in eax
	mov ecx, eax
	mov eax,00000000
	mov ebx,00000000
	
L1: 
	mov al, [edx] ; move a character into al
	add ebx, eax ; add the character ascii value to a counter
	inc edx ; next character
	loop L1

	; ebx now has full ascii value

	add esi,4 ; increment to hash table size
	mov edi, [esi]
	mov eax,ebx ; full ascii value is the divisor
	mov edx, 00000000 ; clear edx to make room for remainder
	div edi ; htptr+4 is the table size
	; remainder stored in edx
	add esi,8 ; go to first element in array

	mov eax,edx
	;eax contains how many slots to increment
	push ebx
	mov ebx, 00000004
	mul ebx ; multiply by dword
	pop ebx
	add esi,eax ; go to the spot in the table we should be at
	;end of segment used from insert
	;esi contains the address that we should look for the key

	mov ebx, [esi]
; check the address of the first node attached to the table 
	cmp ebx,0 ; if the address is 0, there's nothing there --> say we no find anything
	je notFound

repeatSearch:
	mov edx, [ebx] ; address of node key
	mov edi, ebx ; store in case need return
	mov eax, htkey ; address of param key
	INVOKE Str_compare, edx, eax ; compare key we're looking at and our target key
	jne continueSearch 

	; past this point we found the value we want to return
    add ebx, 4 ; get address of value in second slot of node
    mov edx, [ebx];
	mov eax, 00000001
	jmp endSearch

continueSearch:
	add ebx, 8
	mov edx, ebx
	mov ebx, [edx] ; put address of next node in ebx
    cmp ebx, 0 ; if the next node is blank, there is nothing left to search
    je notFound 
        
    ; ebx has the address of the next node
    jmp repeatSearch ; keep searching
notFound:
	mov eax,00000000 
	mov edx, offset msgKeyNotFound
	call WriteString
	
	mov edi, OFFSET msgKeyNotFound
	
	ret
endSearch:
	push edx
	mov edx, offset msgKeyFound
	call WriteString
	call crlf
	mov edx, offset msgKey ; preparation message
	call WriteString

	mov edx, htkey ; print key 
	call WriteString 
	call crlf
	mov edx, offset msgValue ;print value
	call WriteString
	pop edx
	call WriteString
	call crlf
	
	pop ebx
	add edi, 4
	mov edx, edi

	; address of value will be in edx upon return
	; value will be in edi

	mov edi, [edx]

	ret
HTSearch ENDP


	

;------------------------------------
HTPrint PROC uses ebx ecx esi edx,
	htptr:DWORD
; inputs - a pointer to the hash table
; returns - nothing
	call crlf
	mov esi, htptr ; esi holds pointer to table


	mov edx, offset msgTableName ; preparation message
	call WriteString
	mov edx, [esi]; table name is the first slot
	call WriteString ; print the name of the table
	call crlf
	
	

	add esi,4
	mov edx, offset msgTableSize ; preparation message
	call WriteString
	mov ecx,[esi] ; move number of elements into ecx to loop through
	mov eax,[esi] ; second index is the table size
	call WriteInt ; print table size
	
	
	call crlf

	add esi,4 ; esi now at third index is the number of elements in the table;
	mov edx, offset msgElementsInTable ; preparation message
	call WriteString
	mov eax,[esi] 
	call WriteInt ; print the number of elements in the hash table
	call crlf
	add esi,4
	; now esi is at the first element

	mov eax, 0 ; use EAX to store the hash index


incTime:
	mov ebx,[esi] ; get value at slot, store in ebx

	cmp ebx,0 ; check if slot is empty

	jne notEmpty 

	call WriteInt ; print current index 
	mov edx, offset msgArrow
	call WriteString
	mov edx, OFFSET msgNothing
	call WriteString
	jmp endOfLoop
NotEmpty:
	call WriteInt ; print the index that we're at
	mov edx, offset msgArrow
	call WriteString

printingLink:	
	;past this point we need to print contents of a node

	mov edx,[ebx] ; move key address into edx
	call WriteString ; this prints the key
	mov edx, offset msgColon
	call WriteString
	
	add ebx,4 ; inc to the value
	mov edx,[ebx]
	call WriteString ; this prints the value

	
	

	
	add ebx,4
	mov edx,ebx ; move nxt address into eax. no need to dereference since we simply want the address stored there
	mov ebx,[edx] ; now ebx holds the first address of the next link

	cmp ebx,0 ; if the next link is 0
	je endOfLoop ;  we can stop going through the linked list

	;otherwise,
	mov edx, offset msgArrow
	call WriteString
	jmp printingLink

	

endOfLoop:
	add esi,4 ; increment hash index
	inc eax ; inc the hash index to display
	call crlf
	loop incTime


	


	ret
HTPrint ENDP


	

;------------------------------------
HTDestroy PROC uses ebx ecx esi edx, ; NOT IMPLEMENTED
	htptr:DWORD, hthheap:HANDLE
; destroy the hash table, freeing up any heaps allocated
; inputs - a pointer to the hash table
; returns - nothing
	
    ret
HTDestroy ENDP

END main