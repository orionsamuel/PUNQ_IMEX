c
c	Estatística dos modelos que estao dentro de um certo nivel de 
c	inundacao
c
	subroutine estat_inund(jpor, por_min, n_inund,DIR,NF,ESTINUND,
	1	active_blocks, por_max, x_inund, sum_por, avg_por,
	1	g, h, k,l,m, n_l, sum_std, std_por,
     1	int_real_active,x_real_total,n_active_blocks,n_prop)
c
	real media_por(n_prop*n_l),desvp_por(n_prop*n_l),g(n_prop*n_l),
	1h(n_prop*n_l),x_inund(40*k,n_prop*n_l),
     1int_real_active(n_prop*n_l),x_real_total(n_prop*n_active_blocks)
	character*80 ESTINUND
	character*80 DIR
	integer NF,n_active_blocks
	
	real active_blocks(n_prop*n_l), sum_por, sum_std
	integer n_inund
		
		write(*,*) 'inundados', n_inund
		do ll=1, n_prop*n_l
	
				jpor= ll
				por_min=1.0E+38
				por_max=-1.0E+38
				sum_por=0.0
			
			if(active_blocks(ll).EQ.1) then
				do kkk=1,n_inund
					if(por_min.GT.x_inund(kkk,jpor))
	1					por_min=x_inund(kkk,jpor)
					if(por_max.LT.x_inund(kkk,jpor))
	1					por_max=x_inund(kkk,jpor)
					sum_por=sum_por+x_inund(kkk,jpor)
		
			enddo
			
	
				media_por(jpor) = sum_por/n_inund
				g(jpor)=por_min
				h(jpor)=por_max
				sum_std=0.0
				do kkk=1,n_inund
					sum_std = sum_std +
	1				(media_por(jpor)-x_inund(kkk,jpor))**2
				enddo
				desvp_por(jpor)=sqrt(sum_std/(n_inund-1))
			endif
			
	enddo
	open(22,file=DIR(:NF)//ESTINUND,status='unknown')
	do jjj=1, n_active_blocks
		write(22,1090) jjj, media_por(jjj),desvp_por(jjj), g(jjj),
     .h(jjj), x_real_total(int_real_active(jjj))
	enddo
1090	format(i5,1x,19(f10.4,2x))
	close(22)
	return
	end