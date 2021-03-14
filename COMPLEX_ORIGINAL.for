      subroutine const_max(Npocos, n_active_blocks, int_active,
     1	DIR, NF, MODINUND_FLAG,
	1	HOTSTARTDAT, HOTSTART_DAT_FLAG,HOTSTARTOUT,HOTSTART_OUT_FLAG,
     1	PORODAT, PERMXDAT, PERMZDAT, PUNQS3DAT,
	1	PUNQS3_RPT_WCUT_DAT, PUNQS3_RPT_BHP_DAT, PUNQS3_RPT_GOR_DAT,
	1	PUNQS3_RPT_WCUT_OUT, PUNQS3_RPT_BHP_OUT, PUNQS3_RPT_GOR_OUT,
     1	ERRORDAT,Nmed_wcut, Nmed_bhp, Nmed_gor, Npor, active_blocks,
	1	wcut_obs, wcut_cal, bhp_obs, n_l,
     1	bhp_cal, gor_obs, gor_cal,
     1	por, inund, x_inund,
     1	k0, kactive, k, l, m,n,itmax,
     1	std_k, std_wcut, std_bhp, std_gor, N_pairs,lambda_por_glb,
     1	alpha,beta,gamma,delta,x,r,f,iev1,iev2,xc,g,h,it,flag,n_prop)
c     Purpose:
c
c     To find the constrained maximum of a function of several
c     variables by the complex method of M.J.Box. This is the
c     primary subroutine and coordinates the special purpose
c     subroutines (CHECK,CENTROID,FUNC,CONSTRAINTS). Initial 
c     guess of the independent variables, random numbers, solution 
c     parameters, dimension limits and printer code designition
c     are obtained from the main program. Final function and 
c     independent variable values are transferred to the main
c     program for print out. Intermediate printouts are 
c     provided in this subroutine. The user must provide the main
c     program and the subroutines that specify the fuction
c     (FUNC) and constraints (CONSTRAINTS).
c
c     Usage:
c
c     call const_max(k0,kd,ld,md,nd,k,m,n,itmax,alpha,beta,gamma,
c     delta,x,r,f,iev1,iev2,xc,flag)
c
c     subroutines required:
c
c     check(kd,ld,md,nd,m,n,x,g,h,i,kode,xc,delta,k1)
c     checks all points against explicit and implicit
c     constraints and apllies correction if violations
c     are found.
c
c     centroid(kd,ld,nd,n,k1,x,iev1,xc)
c     calculates the centroid of points
c
c     func_comp(kd,x,f,i, etc)
c     specifies objective function on a simplex point
c     (user supplied)
c
c     Description of parameters:
c
c     INPUT PARAMETERS:
c
c     k0 = printer unit number
c
c     kd = dimension associated with number of points in the simplex
c          (kd.ge.nd+1)
c
c     ld = dimension associated with total number of independent 
c          variables (explicit + implicit)
c
c     md = dimension associated with number of sets of constraints
c
c     nd = dimension associated with number of explicit independent 
c          variables 
c
c     k  = number of points in the simplex (k.ge.n+1)
c
c     l = total number of independent variables 
c         (explicit + implicit)
c
c     m = number of sets of constraints
c
c     n = number of explicit independent variables 
c
c     itmax = maximum number of iteractions
c
c     alpha = reflection factor
c
c     beta, gamma = convergence parameters
c
c     delta = explicit constraint violation correction
c
c     x(k,l) = independent variables
c              define initial values in line 1 
c
c     r(k,n) = random numbers between 0 e 1
c
c     OUTPUT PARAMETERS:
c
c     x(k,l) =solution points for independent variables
c
c     f(k) = objective function calculate on each point in the simplex
c            defined in subroutine func 
c
c     iev1 = index of point with minimum function value
c            defined in subroutines const_max and check
c
c     iev2 = index of point with maximum function value
c            defined in subroutine const_max
c
c     xc = centroid defined in subroutine centroid
c
c     g(md) = defined lower constraint 
c
c     h(md) = defiend upper constraint
c
c     it = iteraction index defined in subroutine const_max 
c     flag = identification of output status
c            if flag=0, a maximum was founded satisfying convergence criteria
c                        and maximum number of iteractions
c            if flag=1, the maximum number of iteractions was exceded
c
c     LOCAL PARAMETERS:
c
c     kode = key used to determine if implicit constraints
c            are provided defined in subroutines const_max and check
c
c     k1 = do loop limit defined in subroutine const_max
c
c     i = identification of momentaneous simplex's point
c
c------------------------------------------------------------------------------
c
	use DFPORT
	character*200 DIR, PORODAT, PERMXDAT, PERMZDAT,PUNQS3DAT,
     1PUNQS3_RPT_WCUT_DAT, PUNQS3_RPT_WCUT_OUT, PUNQS3_RPT_BHP_DAT,
     1PUNQS3_RPT_BHP_OUT, PUNQS3_RPT_GOR_DAT, PUNQS3_RPT_GOR_OUT,
     1cmd, ERRORDAT, HOTSTARTDAT, HOTSTARTOUT
	LOGICAL MODINUND_FLAG, HOTSTART_DAT_FLAG, HOTSTART_OUT_FLAG
	integer i, NF, n_active_blocks, flag
	real x(k,l), g(m), h(m), f(k), x_inund(40*k,n_prop*n_l),
     1xc(n), por(n_prop*n_l), active_blocks(n_prop*n_l),r(k,n)
	
	real tempo_wcut(Nmed_wcut),tempo_bhp(Nmed_bhp),
     1tempo_gor(Nmed_gor), wcut_obs(Nmed_wcut,Npocos),
     1bhp_obs(Nmed_bhp,Npocos), gor_obs(Nmed_gor,Npocos),
     1wcut_cal(Nmed_wcut,Npocos), bhp_cal(Nmed_bhp,Npocos),
     1gor_cal(Nmed_gor,Npocos), int_active(n_prop*n_active_blocks+1)

	REAL(4) tempo_cpu
	it=1
      write(k0,995)it
      kode=0
      if(m-n)20,20,10
10    kode=1
20    continue
c
	do ii=2,k
		do j=1,n
			x(ii,j)=.0

		enddo
	enddo
	
	
	
c
c.......calculate complex points and check againts constraints
c.......only for those point which are associated to active blocks
c.......modificacao feita por Aderson do Nascimento, 24/07/2003.
c
	if(HOTSTART_DAT_FLAG) then
		write(*,*)'Lendo o COMPLEX  inicial via HOTSTART'
		go to 945
	endif
	
	write(*,*)'Escrevendo os modelos do COMPLEX no DOCUMENTACAO.DAT'
	write(k0,999)1,(x(1,j), j=1,n)
      do ii=2, k
		do j=1, n
			i=ii
			x(ii,j)=g(j)+r(ii,j)*(h(j)-g(j))
			write(987,*) x(ii,j)
		enddo
			k1=ii
	
			call check(k,l,m,n,x,g,h,i,kode,xc,delta,k1)
				
			write(k0,999)ii,(x(ii,j),j=1,n)

	enddo
c
ccccccccccccccccccccccccccccccc
c	LENDO/ESCREVENDO arquivos nos "hot start"
c
945	if(HOTSTART_DAT_FLAG) then
		write(*,*)'Lendo o arquivo de hot start'
		open(unit=11,file=DIR(:NF)//HOTSTARTDAT,status='unknown')
		
		do ii=1,k
			read(11,*) iii
			read(11,*) (x(iii,j), j=1,n)	
			write(k0,999)iii,(x(iii,j), j=1,n) ! ESCREVENDO NO DOCUMENTA...
		enddo
		do iii=1,k
			read(11,*) ttt
			read(11,*) f(ttt)
		enddo
		write(*,*)'Leitura do arquivo hot start--OK'
		close(11)
	do iii=1,k
		if(MODINUND_FLAG) then
		if(abs(f(iii)).LE.inund) then
		n_inund=n_inund+1
		call inp_x_inund(k, l, n_l, n_inund, por, x_inund, n_prop)
		write(73,*)'Modelos abaixo do nivel de inundacao',inund
		write(73,*)n_inund,'modelo(s) encontrados abaixo deste nivel'
		write(73,*)'Func=',abs(f(iii))
		write(73,*)'Permeabilidades dos modelo inundados'
c		write(73,1050) (x_inund(n_inund,jj),jj=1,n)
		endif
		endif
	enddo
	go to 79
	endif
c
cccccccccccccccccccccccccccccccccccccccccccccccc
	k1=k
	write(k0,*) 'Funcao Objeto do Complex inicial---somente para'
	write(k0,*) 'pontos do complex associados a blocos ativos'
      do i=1,k
	tempo_cpu=SECNDS(0.0)
	call func_comp(k, l, x, i, n_l, 
     1	DIR, NF, PORODAT, PERMXDAT, PERMZDAT, PUNQS3DAT,
	1	PUNQS3_RPT_WCUT_DAT, PUNQS3_RPT_BHP_DAT, PUNQS3_RPT_GOR_DAT,
	1	PUNQS3_RPT_WCUT_OUT, PUNQS3_RPT_BHP_OUT, PUNQS3_RPT_GOR_OUT,
     1	ERRORDAT, Nmed_wcut, Nmed_bhp, Nmed_gor,
     1	Npocos, Npor, por, active_blocks, int_active, n_active_blocks,
     1	wcut_obs, bhp_obs, gor_obs, std_k, std_wcut, std_bhp, std_gor,
     1	N_pairs,lambda_por_glb, Func,n_prop)
	tempo_cpu = SECNDS(tempo_cpu)
	PRINT *, 'Tempo CPU para iteracao:',tempo_cpu, 'segundos'
	f(i)=-Func
	n_inund=0
	
c	Escreve os modelos que estao num certo nivel de inundacao
c
	if(MODINUND_FLAG) then
	if(abs(f(i)).LE.inund) then
		n_inund=n_inund+1
		
		call inp_x_inund(k, l, n_l, n_inund,
     1	por, x_inund, n_prop)
		write(73,*)'Modelos abaixo do nivel de inundacao', inund
		write(73,*)n_inund,'modelo(s) encontrados abaixo deste nivel'
		write(73,*)'Func=',abs(f(i))
		write(73,*)'Permeabilidades dos modelo inundados'
c		write(73,1050) (x_inund(n_inund,jj),jj=1,n)

	endif
	endif
1050  format(10(f9.4,2x))
c
	write(k0,*) 'modelo=', i
	write(k0,*) 'Func=', f(i)
	write(*,*) 'modelo=', i
	write(*,*) 'Func=', f(i)
	enddo
	
c	
79	do ii=1,k
	write(k0,*) 'modelo=', ii
	write(k0,*) 'Func=', f(ii)
	enddo

	if(HOTSTART_OUT_FLAG) then
	write(*,*)'Escrevendo no Hot start de saida'
	open(unit=12,file=DIR(:NF)//HOTSTARTOUT,status='unknown')
		do ii=1,k
			write(12,*) ii
			write(12,9991) (x(ii,j), j=1,n)
		enddo
	endif
	if(HOTSTART_OUT_FLAG) then
		do ii=1,k
			write(12,*) ii
			write(12,*) f(ii)	
		enddo
	endif
	close(12)
      kount=1
      ia=0
c.......find point with lowest function value
      write(k0,998)(f(i),i=1,k)
80    iev1=1
      do 100 icm=2, k
         if(f(iev1)-f(icm)) 100,100,90
90       iev1=icm
100   continue
c.......find point with highest function value
      iev2=1
      do 120 icm=2, k
         if(f(iev2)-f(icm))110,110,120
110      iev2=icm
120   continue
c.......check convergence criteria
      if(f(iev2)-(f(iev1)+beta))140,130,130
130   kount=1
      go to 150
140   kount=kount+1
      if(kount-gamma)150,240,240
c.......replace point with lowest function value
150   call centroid(k,l,n,k1,x,iev1,xc)
      do 160 j=1,n
         x(iev1,j)=(1.+alpha)*(xc(j))-alpha*(x(iev1,j))
160   continue
      i=iev1
      call check(k,l,m,n,x,g,h,i,kode,xc,delta,k1)
	tempo_cpu=SECNDS(0.0)
     	call func_comp(k, l, x, i, n_l,
     1	DIR, NF, PORODAT, PERMXDAT, PERMZDAT, PUNQS3DAT,
	1	PUNQS3_RPT_WCUT_DAT, PUNQS3_RPT_BHP_DAT, PUNQS3_RPT_GOR_DAT,
	1	PUNQS3_RPT_WCUT_OUT, PUNQS3_RPT_BHP_OUT, PUNQS3_RPT_GOR_OUT,
     1	ERRORDAT, Nmed_wcut, Nmed_bhp, Nmed_gor,
     1	Npocos, Npor, por, active_blocks, int_active, n_active_blocks,
     1	wcut_obs, bhp_obs, gor_obs, std_k, std_wcut, std_bhp, std_gor,
     1	N_pairs,lambda_por_glb, Func,n_prop)
	tempo_cpu = SECNDS(tempo_cpu)
	PRINT *, 'Tempo CPU para iteracao:',tempo_cpu, 'segundos'
	f(i)=-Func

c
c	Escreve os modelos que estao num certo nivel de inundacao

	if(MODINUND_FLAG) then
	if(abs(f(i)).LE.inund) then
		n_inund=n_inund+1
		call inp_x_inund(k, l, n_l, n_inund,
     1	por, x_inund, n_prop)
		write(73,*)'Modelos abaixo do nivel de inundacao',inund
		write(73,*)n_inund,'modelo(s) encontrados abaixo deste nivel'
		write(73,*)'Func=',abs(f(i))
		write(73,*)'Permeabilidades dos modelos'
c		write(73,1050) (x_inund(n_inund,jj),jj=1,n)
	endif
	endif
c
	write(k0,*) 'Identificacao da iteracao=',it
	write(k0,*) 'Tentativa de nova funcao para modelo',i,'Func=',-f(i)
c.......replace new point if it repeats as lowest function value
170   iev3=1
      do 190 icm=2,k
         if(f(iev3)-f(icm))190,190,180
180      iev3=icm
190   continue
      if(iev3-iev1)220,200,220
200   do 210 jj=1,n
           x(iev1,jj)=(x(iev1,jj)+xc(jj))/2.
210   continue
      i=iev1
      call check(k,l,m,n,x,g,h,i,kode,xc,delta,k1)
	tempo_cpu=SECNDS(0.0)
      
	call func_comp(k, l, x, i,n_l,
     1	DIR, NF, PORODAT, PERMXDAT, PERMZDAT, PUNQS3DAT,
	1	PUNQS3_RPT_WCUT_DAT, PUNQS3_RPT_BHP_DAT, PUNQS3_RPT_GOR_DAT,
	1	PUNQS3_RPT_WCUT_OUT, PUNQS3_RPT_BHP_OUT, PUNQS3_RPT_GOR_OUT,
     1	ERRORDAT, Nmed_wcut, Nmed_bhp, Nmed_gor,
     1	Npocos, Npor, por, active_blocks, int_active, n_active_blocks,
     1	wcut_obs, bhp_obs, gor_obs, std_k, std_wcut, std_bhp, std_gor,
     1	N_pairs,lambda_por_glb, Func, n_prop)
	tempo_cpu = SECNDS(tempo_cpu)
	
	PRINT *, 'Tempo CPU para iteracao:',tempo_cpu, 'segundos'
	f(i)=-Func
	
c
c	Escreve os modelos que estao num certo nivel de inundacao

	if(MODINUND_FLAG) then
	if(abs(f(i)).LE.inund) then
		n_inund=n_inund+1
		call inp_x_inund(k, l, n_l, n_inund,
     1	por, x_inund, n_prop)
		write(73,*)'Modelos abaixo do nivel de inundacao',inund
		write(73,*)n_inund,'modelo(s) encontrados abaixo deste nivel'
		write(73,*)'Func=',abs(i)
		write(73,*)'porosidade dos modelos'
c		write(*,*) (x_inund(n_inund,jj),jj=1,n)
c		write(73,1050) (x_inund(n_inund,jj),jj=1,n)
	
	endif
	endif
c
c
	
	write(k0,*) 'Identificacao da iteracao=',it
	write(k0,*) 'Tentativa de nova funcao para modelo',i,'Func=',f(i)
      go to 170
220   continue
      write(k0,997)(x(iev1,jb),jb=1,n)
      write(k0,998)(f(i),i=1,k)
      write(k0,996)(xc(j),j=1,n)
c
	write(*,*)'******'
	write(*,*)'ITERACAO=', it
	write(*,*)'******'
      write(*,*)'it = ',it, '  Melhor valor: E = ',abs(f(iev2))
17    format(f10.4, 100(1x,f5.0))
c
*******************************************************************************************	
	
	if(mod(it,10).EQ.0)then
	write(*,*)'****Escrevendo no arquivo HOTSTART.DAT*****'
	open(unit=30,file='c:\temp_flavio001\HOTSTART.DAT',status='unknown')
		write(30,9993) it
9993		format(i4,1x,'Iteracao')    
		do ii=1,k
			write(30,*) ii
			write(30,9991) (x(ii,j), j=1,n)
9992			format(f10.4)
	enddo
	do ii=1,k
			write(30,*) ii
			write(30,*) f(ii)	
		enddo
	endif
	close(30)

*******************************************************************************************
      it=it+1
      if(it-itmax)230,230,250
230   continue
      write(k0,995)it
      go to 80
240   flag=0
      return
250   flag=1
      return
999   format(1h , 15x, 21h coordinates at point, i5/10(f9.4, 2x))
9991  format(f10.4)
998   format(1h , 20x, 16h function values, /10(E10.4, 2x))
997   format(1h , 20x, 16h corrected point, /10(f8.4, 2x))
996   format(1h , 21h centroid coordinates, 2x, 10(f8.4, 2x))
995   format(1h , //10h iteration, 4x, i5)
      end

	
c------------------------------------------------------------------------------
      subroutine check(k,l,m,n,x,g,h,i,kode,xc,delta,k1)
c
c     purpose:
c
c     To check all points against the explicit and implict constraints
c     and to apply corrections if violations are found
c
c     Usage:
c
c     call check(kd,ld,md,nd,m,n,x,g,h,i,kode,xc,delta,k1)
c
c     subroutines required:
c
c     centroid(kd,ld,nd,n,k1,x,iev1,xc)
c
c-------------------------------------------------------------------------------
c
c      dimension x(k,l),g(m),h(m),xc(n)
	real x(k,l),g(m),h(m),xc(n)
c
10    kt=0
c.....check against explicit constraints
c     DELTA TEM AGORA FUNCAO MULTIPLICATIVA 
c
      do 50 j=1,n
         
	   if(x(i,j)-g(j))20,20,30
20       x(i,j)=g(j)+delta*abs(x(i,j))
         go to 50
30       if(h(j)-x(i,j))40,40,50
40       x(i,j)=h(j)-delta*abs(x(i,j))
50    continue
      if(kode)110,110,60
c.......check against implicit constraints
60    continue
      nn=n+1
      do 100 j=nn,m
         if(x(i,j)-g(j))80,70,70
70       if(h(j)-x(i,j))80,100,100
80       iev1=i
         kt=1
         call centroid(k,l,n,k1,x,iev1,xc)
         do 90 jj=1,n
            x(i,jj)=(x(i,jj)+xc(jj))/2.
90       continue
100   continue
      if(kt)110,110,10
110   return
      end
c--------------------------------------------------------------------------------
      subroutine centroid(k,l,n,k1,x,iev1,xc)
c
c     Purpose:
c
c     to calculate the centroid of points
c
c     Usage:
c
c     call centroid(kd,ld,nd,n,k1,x,iev1,xc)
c
c---------------------------------------------------------------------------------
c
c      dimension x(k,l),xc(n)
	real x(k,l),xc(n)
c
      do 20 j=1,n
         xc(j)=0.
         do 10 il=1,k1
            xc(j)=xc(j)+x(il,j)
10       continue
         rk=k1
         xc(j)=(xc(j)-x(iev1,j))/(rk-1.)
20    continue
      return
      end