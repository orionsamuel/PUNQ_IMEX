	real inf_por(1400), sup_por(1400),por_truth(1400)
	real por(700), permh(700), caixa, permH_truth(1400)


	open(1,file='PORO_TRUTH.DAT',status='unknown')
	open(2,file='PERMX_TRUTH.DAT',status='unknown')

	n_l=700

	do i=1, n_l
		
		read(1,*) por(i)
		read(2,*) permh(i)
	
	enddo

	do ii=1,n_l
		if(por(ii).EQ.0.0)then
			goto 15
		else

c	camadas 1, 3 e 5		
c	if((ii.LE.140).or.((ii.GT.280).and.(ii.LE.420)).or.
c	1((ii.GT.560).and.(ii.LE.n_l)))then
c		
c			if(por(ii).GE.0.25)then
c				inf_por(ii)=0.25
c				sup_por(ii)=0.3
c				inf_por(ii+n_l)=800
c				sup_por(ii+n_l)=1000
c			endif
c			if(por(ii).LT.0.25.and.por(ii).GE.0.20)then
c				inf_por(ii)=0.20
c				sup_por(ii)=0.249
c				inf_por(ii+n_l)=600
c				sup_por(ii+n_l)=799.9
c			endif
c			if(por(ii).LT.0.20.and.por(ii).GE.0.15)then
c				inf_por(ii)=0.15
c				sup_por(ii)=0.199
c				inf_por(ii+n_l)=400
c				sup_por(ii+n_l)=599.9
c			endif
c			if(por(ii).LT.0.15.and.por(ii).GE.0.10)then
c				inf_por(ii)=0.10
c				sup_por(ii)=0.149
c				inf_por(ii+n_l)=200
c				sup_por(ii+n_l)=399.9
c			endif
c			if(por(ii).LT.0.10.and.por(ii).GE.0.05)then
c				inf_por(ii)=0.05
c				sup_por(ii)=0.099
c				inf_por(ii+n_l)=100
c				sup_por(ii+n_l)=199.9
c			endif
c			if(por(ii).LT.0.05.and.por(ii).GE.0.01)then
c				inf_por(ii)=0.01
c				sup_por(ii)=0.049
c				inf_por(ii+n_l)=0.5
c				sup_por(ii+n_l)=99.9
c			endif
c	endif
c
cc	camada 2
c		if((ii.GT.140).and.(ii.LE.280)) then
c			inf_por(ii)=0.01
c			sup_por(ii)=0.15
c			inf_por(ii+n_l)=0.5
c			sup_por(ii+n_l)=150
c		endif
c
cc	camada 4
c		if((ii.GT.420).and.(ii.LE.560)) then
c				inf_por(ii)=0.01
c				sup_por(ii)=0.05
c				inf_por(ii+n_l)=0.5
c				sup_por(ii+n_l)=100
c			if((por(ii).LT.0.17).and.(por(ii).GT.0.13))then
c				inf_por(ii)=0.13
c				sup_por(ii)=0.17
c				inf_por(ii+n_l)=300
c				sup_por(ii+n_l)=500
c			endif
c
c			if(por(ii).LT.0.05)then
c				inf_por(ii)=0.01
c				sup_por(ii)=0.05
c				inf_por(ii+n_l)=0.5
c				sup_por(ii+n_l)=100
c			endif
c		endif
c
c	enddo


	caixa=0.6

					
			inf_por(ii) = por(ii) - por(ii)*caixa			
			if(inf_por(ii).LT.0.01)then
				inf_por(ii)=0.01
			endif
				
			sup_por(ii) = por(ii) + por(ii)*caixa
			if(sup_por(ii).GT.0.3)then
				sup_por(ii)=0.3
				endif
		
			inf_por(ii+n_l) = permh(ii) -
	1		 permh(ii)*caixa
			if(inf_por(ii+n_l).LT.0.5)then
				inf_por(ii+n_l)=0.5
			endif
	
			sup_por(ii+n_l) = permh(ii) + 
	1		permh(ii)*caixa
			if(sup_por(ii+n_l).GT.1000)then
			sup_por(ii+n_l)=1000
			endif
	

15	endif
	enddo
c	Os limites de variacao nos blocos com pocos e de 2% do valor verdadeiro

	inf_por(58)= 0.09016
	inf_por(198)=0.098
	inf_por(338)=0.09114
	inf_por(478)=0.196
	inf_por(618)=0.2254

	inf_por(59)= 0.1568
	inf_por(199)=0.098
	inf_por(339)=0.1274
	inf_por(479)=0.1274

	inf_por(85)= 0.1666
	inf_por(225)=0.049
	inf_por(365)=0.0784
	inf_por(505)=0.1372
	inf_por(645)=0.2352

	inf_por(105)=0.0588
	inf_por(245)=0.0882
	inf_por(385)=0.1176
	inf_por(525)=0.1568
	inf_por(665)=0.2548

	inf_por(109)=0.2744
	inf_por(249)=0.0882
	inf_por(389)=0.1666
	inf_por(529)=0.1568

	inf_por(116)=0.0588
	inf_por(256)=0.0686
	inf_por(396)=0.1862
	inf_por(536)=0.1078

	sup_por(58)= 0.09384
	sup_por(198)=0.102
	sup_por(338)=0.09486
	sup_por(478)=0.204
	sup_por(618)=0.2346

	sup_por(59)= 0.1632
	sup_por(199)=0.102
	sup_por(339)=0.1326
	sup_por(479)=0.1326

	sup_por(85)= 0.1734
	sup_por(225)=0.051
	sup_por(365)=0.0816
	sup_por(505)=0.1428
	sup_por(645)=0.2448

	sup_por(105)=0.0612
	sup_por(245)=0.0918
	sup_por(385)=0.1224
	sup_por(525)=0.1632
	sup_por(665)=0.2652

	sup_por(109)=0.2856
	sup_por(249)=0.0918
	sup_por(389)=0.1734
	sup_por(529)=0.1632

	sup_por(116)=0.0612
	sup_por(256)=0.0714
	sup_por(396)=0.1938
	sup_por(536)=0.1122

	inf_por(n_l+58)=29.0178
	inf_por(n_l+198)=68.6784
	inf_por(n_l+338)=75.4992
	inf_por(n_l+478)=234.906
	inf_por(n_l+618)=722.064

	inf_por(n_l+59)=567.616
	inf_por(n_l+199)=23.814
	inf_por(n_l+339)=227.556
	inf_por(n_l+479)=144.942

	inf_por(n_l+85)=239.512
	inf_por(n_l+225)=15.4546
	inf_por(n_l+365)=48.8824
	inf_por(n_l+505)=247.548
	inf_por(n_l+645)=704.424

	inf_por(n_l+105)=12.7792
	inf_por(n_l+245)=69.9622
	inf_por(n_l+385)=337.022
	inf_por(n_l+525)=474.712
	inf_por(n_l+665)=583.198

	inf_por(n_l+109)=756.07
	inf_por(n_l+249)=34.1628
	inf_por(n_l+389)=261.758
	inf_por(n_l+529)=379.652
	
	inf_por(n_l+116)=18.6494
	inf_por(n_l+256)=25.1762
	inf_por(n_l+396)=517.636
	inf_por(n_l+536)=153.762
	
	sup_por(n_l+58)=30.2022
	sup_por(n_l+198)=71.4816
	sup_por(n_l+338)=78.5808
	sup_por(n_l+478)=244.494
	sup_por(n_l+618)=751.536

	sup_por(n_l+59)=590.784
	sup_por(n_l+199)=24.786
	sup_por(n_l+339)=236.844
	sup_por(n_l+479)=150.858

	sup_por(n_l+85)=249.288
	sup_por(n_l+225)=16.0854
	sup_por(n_l+365)=50.8776
	sup_por(n_l+505)=257.652
	sup_por(n_l+645)=733.176

	sup_por(n_l+105)=13.3008
	sup_por(n_l+245)=72.8178
	sup_por(n_l+385)=350.778
	sup_por(n_l+525)=494.088
	sup_por(n_l+665)=607.002

	sup_por(n_l+109)=786.93
	sup_por(n_l+249)=35.5572
	sup_por(n_l+389)=272.442
	sup_por(n_l+529)=395.148
	
	sup_por(n_l+116)=19.4106
	sup_por(n_l+256)=26.2038
	sup_por(n_l+396)=538.764
	sup_por(n_l+536)=160.038

	open(3,file='C:\temp_flavio001\inf_por.dat',status='unknown')
	open(4,file='C:\temp_flavio001\sup_por.dat',status='unknown')
	
	do i=1, 2*n_l
		write(3,*) inf_por(i)
		write(4,*) sup_por(i)
	enddo
	close(3)
	close(4)

	stop
	end

