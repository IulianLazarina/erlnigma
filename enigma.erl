%%%% Theory of Distributed Systems  Assignment  -  Erl-nigma
%%%% Part 2


-module(enigma).
%-export ([setup/5, crypt/2, kill/0, f_plug/2,run2/0,rotor/10]).  %  this method gives a bunch of errors when running

-include("Enigma.hrl"). % file with all the rotors and reflectors
-include_lib("eunit/include/eunit.hrl"). %for assertEqual


-compile(export_all). 



% function for all the rotors
rotor (C,P,L,R,Inc_l,Inc_r,Rotor,Ring,Next,Previous) ->

		case C of 
		 
			26 ->    receive       % when C is 26 
							{Inc_r, 1} -> 
									Next ! {Inc_l, 1}, %send another increment
									receive
										{R, X, From} -> Next ! {L, f_rotor(X,contain(P+1),Rotor,Ring), From} %send correct letter on channel L
									end,
									rotor(1,contain(P+1),L,R,Inc_l,Inc_r,Rotor,Ring,Next,Previous);   %reset C to 1 and increment p
		
							{Inc_r, 0} -> 
									Next ! {Inc_l, 0}, %send increment 0 
									receive
										{R, X, From} -> Next ! {L, f_rotor(X,P,Rotor,Ring), From} %send correct letter on channel L
									end,
									rotor(26,P,L,R,Inc_l,Inc_r,Rotor,Ring,Next,Previous);  
	
							{Inc_l, 0} -> 
									Previous ! {Inc_r, 0},
									receive
										{L, X, From} -> Previous ! {R, f_rotorbar(X,P,Rotor,Ring), From} %send correct letter on channel R
									end,
									rotor(26,P,L,R,Inc_l,Inc_r,Rotor,Ring,Next,Previous)
					end;
					
			_ -> 	receive %otherwise
							{Inc_r, 1} -> 
									Next ! {Inc_l, 0},
									receive
										{R, X, From} -> Next ! {L, f_rotor(X,contain(P+1),Rotor,Ring), From} %send correct letter on channel L
									end,
									rotor(C+1,contain(P+1),L,R,Inc_l,Inc_r,Rotor,Ring,Next,Previous);  
		
							{Inc_r, 0} -> 
									Next ! {Inc_l, 0},
									receive
										{R, X, From} -> Next ! {L, f_rotor(X,P,Rotor,Ring), From} %send correct letter on channel L
									end,
									rotor(C,P,L,R,Inc_l,Inc_r,Rotor,Ring,Next,Previous);
	
							{Inc_l, 0} -> 
									Previous ! {Inc_r, 0},
									receive
										{L, X, From} -> Previous ! {R, f_rotorbar(X,P,Rotor,Ring), From} %send correct letter on channel R
									end,
									rotor(C,P,L,R,Inc_l,Inc_r, Rotor,Ring,Next,Previous)
					end
		end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%  the way the rotors work is better explained at https://math.dartmouth.edu/~jvoight/Fa2012-295/EnigmaSimManual.pdf  %%%%%%%%

f_rotor(X,P,Rotor,Ring) ->

	Newletter = X + P - Ring , 			% configure the right letter in the rotor depending on p and ring setting

	if                                  % make sure we keep things in capital letters
		(Newletter > $Z) -> 
			Temp = Newletter - 26;
	    (Newletter < $A) -> 
			Temp = Newletter + 26;
		
		true->
			Temp = Newletter
	end,

	Temp2 = rotorhelper(Rotor,Temp) - P + Ring ,    % configure the right letter out of the rotor depending on p and ring setting after switching letters in the rotor
	
	if 
		(Temp2 > $Z) ->
			Final = Temp2 - 26;
	    (Temp2 < $A) ->
			Final = Temp2 + 26;
		
		true->
			Final = Temp2
	end.


% similar to f_rotor but we are encrypting on the way back from the reflector
f_rotorbar(X,P,Rotor,Ring) ->
	Newletter = X + P  - Ring ,

	if 
		(Newletter > $Z) -> 
			Temp = Newletter - 26;
	    (Newletter < $A) -> 
			Temp = Newletter + 26;
		
		true->
			Temp = Newletter
	end,
	
	Temp2 = rotorhelperbar(Rotor,Temp)  - P  + Ring ,
	
	if 
		(Temp2 > $Z) ->
			Final = Temp2 - 26;
	    (Temp2 < $A) ->
			Final = Temp2 + 26;
		
		true->
			Final = Temp2
	end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5


%%helper functions for f_rotor and f_rotorbar

%switch two letters in a pair from the rotor
rotorhelper (Rotors, A) ->
	case Rotors of
		[{A,B}|T] ->  
			B;
		[_|T] ->  
			rotorhelper(T,A);
		[] ->  
			A
	end.

%switch two letters in a pair from the rotor
%reversed order in the tuple because we are on the way back 
rotorhelperbar (Rotors, A) ->
	case Rotors of
		[{B,A}|T] -> 
			B;
		[_|T] ->  
			rotorhelperbar(T,A);
		[] ->  
			A
	end.


%function to make sure we keep P between 1 and 26, resetting it after 26
contain (A)->
	if
		A > 26 -> 
			1;
		true -> 
			A
	end.


%reflector 
reflector (In, Out,Inc, Reflector) ->
	receive
		{In, X, From} -> 
					rotor1 ! {Inc, 0}, %send increment 0 on the way back
					rotor1 ! {Out, f_reflector(Reflector, X), From} %send the correct letter back 
						 
		end,
		reflector (In, Out,Inc, Reflector).

%Switches letters that are in a pair in the reflector
f_reflector (Reflector,A) -> 
	case Reflector of
		[{A,B}|T] -> 
			B;
		[{B,A}|T] ->
			B;
		[_|T] -> 
			f_reflector(T,A);
		[] -> 
			A
	end.

%keyboard
%recieves letter one by one send them to be encrypted
keyboard (Key, Lamp, Inc) ->
	receive 
		{crypt, Key2, From} ->  
						rotor3! {Inc, 1}, %send increment 1 to the first rotor
						plugboard ! {Key, Key2, From}, % send the key to the plugboard
							    
						receive
						   {Lamp, Y, From} -> From! {Y, self()}  %wait for the encrypted key as the lamp signal
						end		
	end,
	keyboard(Key, Lamp, Inc).
	
	
%send the letters from the message one by one to the keyboard
sendkeys(PID, Message, EncodedMessage) ->
  	case Message of
		
		[H|T] -> 
			keyboard! {crypt, H, self()},
					
			receive
				{CypherKey, From} -> 
							sendkeys(PID,T,[CypherKey|EncodedMessage])
			end;
			
		[] -> 
			lists:reverse(EncodedMessage) %reverse the final string as we recieve the encrypted letters in reverse order
	end.	

	
	
% plugboards can recieve and send letters after they have been switched
plugboard (R,L,Plugboard) ->
	receive
		{R, X, From} -> 
						rotor3 ! {L, f_plug(Plugboard,X), From};
					
		
		{L, X, From} -> 
						keyboard ! {R, f_plug(Plugboard,X), From}
						
	end,
		plugboard(R,L,Plugboard).

%switch two letters that are in a pair in the plugboard
f_plug (Plugboard, A) ->
	case Plugboard of
		[{A,B}|T] ->
			B;
		[{B,A}|T] ->
			B;
		[_|T] ->
			f_plug(T,A);
		[] ->
			A
	end.



% Set up an enigma machine
setup(Reflector,Rotors,Rings,Plugboard, Init) ->
	
	
	% list of all the rotors from "enigma.hrl" with notches 
	RotorList=[
		{"I",rotorI(),$Q},   %{rotor name, rotor, notch position} ,the notch is the place where we increment the next rotor
		{"II",rotorII(),$E},
		{"III",rotorIII(),$V},
		{"IV",rotorIV(),$J},
		{"V",rotorV(),$Z},
		{"VI",rotorVI(),$M},   % TODO: make rotors able to have multiple notches
		{"Beta",rotorBeta(),$M},
		{"Gamma",rotorGamma(),$M}],
	
	%list of all the reflectors from "enigma.hrl"
	Reflectors=[
	 	{"A",reflectorA()},
		{"B",reflectorB()},
		{"C",reflectorC()},
		{"ThinB",reflectorThinB()},
		{"ThinC",reflectorThinC()}],

 
	[R1, R2,R3 ] = Rotors,  %set up the rotors

	[L1, L2, L3 ] = Init,   % set up the initial positions

	Init1 = L1 - 64,
	Init2 = L2 - 64,
	Init3 = L3 - 64,
	
	{_,Rotor1,Notch1} =  lists:keyfind(R1,1,RotorList),
	{_,Rotor2,Notch2} =  lists:keyfind(R2,1,RotorList) ,
	{_,Rotor3,Notch3} =   lists:keyfind(R3,1,RotorList) ,
	

	
	C1 = 26 - (Notch1-64) + Init1,   
	C2 = 26 - (Notch2-64) + Init2,   
  	C3 = 26 - (Notch3-64) + Init3, 
	 
	WhichReflector = lists:keyfind(Reflector,1,Reflectors),   %get the right reflector from the list
	{_,R} =  WhichReflector,
	
	[Ring1, Ring2,Ring3 ] = Rings,   %set up the ring settings
	
	
	
	%register all of the processes 
	register(keyboard, spawn(enigma, keyboard,[keys,keys,i1])),
	register(plugboard, spawn(enigma, plugboard,[keys,m3,Plugboard])),
	register(reflector, spawn(enigma, reflector,[ref, ref, e, R])),
	register(rotor1, spawn(enigma, rotor,[C1,Init1,ref, m1, e, i3,Rotor1,Ring1, reflector, rotor2])),
	register(rotor2, spawn(enigma, rotor,[C2,Init2,m1, m2, i3, i2, Rotor2,Ring2, rotor1, rotor3])),
	register(rotor3, spawn(enigma, rotor,[C3,Init3,m2, m3, i2, i1,Rotor3,Ring3,rotor2,plugboard])),
	register(enigma, self()),
	enigma.

%begin encryption
crypt(PID, Message) ->
	ToUpper=[X || X <-  string:uppercase(Message), (X>=$A) and (X=<$Z)],  % make sure we use all capital letters
	sendkeys(PID, ToUpper, "").




%function to unregister all processes
kill()->
		  unregister(rotor1),
		  unregister(rotor2),
		  unregister(rotor3),
		  unregister(keyboard),
		  unregister(plugboard),
		  unregister(reflector),
		  unregister(enigma),
	ok.





%%%%%%%%%%%%%%%% TESTS %%%%%%%%%%%%%%%%%
%% This is a list of tests for the enigma machine, some of them are from the "enigma_eunit.erl" file


%%%%% an enigma machine online simulator was used to compare results and find the proper coded message for guidance %%%%


%test for basic configuration with no plugboards, used early in development
run () ->
	E = setup("B",["III","II","I"],[1,1,1],[],[$A,$A,$A]),
	Res = crypt(E,"this is a test message"),
	kill(),
	?assertEqual("ZPJJSVSPGBWESZRJSS",Res).
	
test () ->
	E = setup("B",["I","II","III"],[6,1,4],[{$B,$K}, {$X,$L}, {$W,$R},{$I,$O}],[$B,$I,$A]),
	Res = crypt(E,"this is a test message"),
	kill(),
	?assertEqual("VVGGSGNXGYRNUOAZNO",Res).
	%Res.

run2 () ->
	Enigma = setup("B",["II","I","III"],[26,23,4],[{$E,$Z}, {$B,$L}, {$X,$P}, {$W,$R}, {$I,$U}, {$V,$M}, {$J,$O}],[$A,$G,$I]),
    Res = crypt(Enigma,"WBCG QLUWJ FCKLW MQIXW PDYVI EIRLY SDQRI ANEQQ QIZRW MIKFW NKZNG SVKZV VWXNB FNQDO"),
	kill(),
	Res.
	
		
%test for encryption and decryption
encryption_decryption () ->
	Enigma = setup("B",["II","I","III"],[26,23,4],[{$E,$Z}, {$B,$L}, {$X,$P}, {$W,$R}, {$I,$U}, {$V,$M}, {$J,$O}],[$A,$G,$I]),
    Res = crypt(Enigma,"WBCG QLUWJ FCKLW MQIXW PDYVI EIRLY SDQRI ANEQQ QIZRW MIKFW NKZNG SVKZV VWXNB FNQDO"),
	kill(),
	?assertEqual("THISISANEXAMPLEMESSAGEXBROUGHTTOYOUBYGCCXQEERSANDHAVEFUNWITHTHEENIGMA",Res),
	Enigma2 = setup("B",["II","I","III"],[26,23,4],[{$E,$Z}, {$B,$L}, {$X,$P}, {$W,$R}, {$I,$U}, {$V,$M}, {$J,$O}],[$A,$G,$I]),
	Res2 = crypt(Enigma2,"THISISANEXAMPLEMESSAGEXBROUGHTTOYOUBYGCCXQEERSANDHAVEFUNWITHTHEENIGMA"),
	kill(),
	?assertEqual("WBCGQLUWJFCKLWMQIXWPDYVIEIRLYSDQRIANEQQQIZRWMIKFWNKZNGSVKZVVWXNBFNQDO",Res2).


%%%% Tests from "enigma_eunit.erl" file	
simple_test() ->
    Enigma = enigma:setup("B",["II","I","III"],[26,23,4],[{$E,$Z}, {$B,$L}, {$X,$P}, {$W,$R}, {$I,$U}, {$V,$M}, {$J,$O}],[$A,$G,$I]),
    Res = enigma:crypt(Enigma,"WBCG QLUWJ FCKLW MQIXW PDYVI EIRLY SDQRI ANEQQ QIZRW MIKFW NKZNG SVKZV VWXNB FNQDO"),
	kill(),
    ?assertEqual("THISISANEXAMPLEMESSAGEXBROUGHTTOYOUBYGCCXQEERSANDHAVEFUNWITHTHEENIGMA",Res).
	
inst_man_test() ->
    Enigma = enigma:setup("A",["II","I","III"],[24,13,22],[{$A,$M}, {$F,$I}, {$N,$V}, {$P,$S}, {$T,$U}, {$W,$Z}],[$A,$B,$L]),
    Res = enigma:crypt(Enigma,"GCDSE AHUGW TQGRK VLFGX UCALX VYMIG MMNMF DXTGN VHVRM MEVOU YFZSL RHDRR XFJWC FHUHM UNZEF RDISI KBGPM YVXUZ"),
	
    ?assertEqual("FEINDLIQEINFANTERIEKOLONNEBEOBAQTETXANFANGSUEDAUSGANGBAERWALDEXENDEDREIKMOSTWAERTSNEUSTADT",Res).

barbarossa_test() ->
    Enigma = enigma:setup("B",["II","IV","V"],[2,21,12],[{$A,$V}, {$B,$S}, {$C,$G}, {$D,$L}, {$F,$U}, {$H,$Z}, {$I,$N}, {$K,$M}, {$O,$W}, {$R,$X}],[$B,$L,$A]),
    Res = enigma:crypt(Enigma,"EDPUD NRGYS ZRCXN UYTPO MRMBO FKTBZ REZKM LXLVE FGUEY SIOZV EQMIK UBPMM YLKLT TDEIS MDICA GYKUA CTCDO MOHWX MUUIA UBSTS LRNBZ SZWNR FXWFY SSXJZ VIJHI DISHP RKLKA YUPAD TXQSP INQMA TLPIF SVKDA SCTAC DPBOP VHJK"),
	kill(),
    ?assertEqual("AUFKLXABTEILUNGXVONXKURTINOWAXKURTINOWAXNORDWESTLXSEBEZXSEBEZXUAFFLIEGERSTRASZERIQTUNGXDUBROWKIXDUBROWKIXOPOTSCHKAXOPOTSCHKAXUMXEINSAQTDREINULLXUHRANGETRETENXANGRIFFXINFXRGTX",Res),
	Enigma2 = enigma:setup("B",["II","IV","V"],[2,21,12],[{$A,$V}, {$B,$S}, {$C,$G}, {$D,$L}, {$F,$U}, {$H,$Z}, {$I,$N}, {$K,$M}, {$O,$W}, {$R,$X}],[$L,$S,$D]),

    Res2 = enigma:crypt(Enigma2,"SFBWDNJUSEGQOBHKRTAREEZMWKPPRBXOHDROEQGBBGTQVPGVKB VVGBI MHUSZ YDAJQ IROAX SSSNR EHYGG RPISE ZBOVM QIEMM ZCYSG QDGRE RVBIL EKXYQ IRGIR QNRDN VRXCY YTNJR"),
	kill(),
    ?assertEqual("DREIGEHTLANGSAMABERSIQERVORWAERTSXEINSSIEBENNULLSEQSXUHRXROEMXEINSXINFRGTXDREIXAUFFLIEGERSTRASZEMITANFANGXEINSSEQSXKMXKMXOSTWXKAMENECXK",Res2).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



