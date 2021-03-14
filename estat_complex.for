c
c	subroutine para fazer a estatísitica dos pontos do complex
c
	subroutine estat_complex(k, jpor, por_min, int_active,
     1	DIR,NF,ESTCOMPLX,
	1	n_active_blocks, active_blocks, por_max, x, sum_por, avg_por,
	1	g, h, l,m, n_l,	sum_std, std_por,x_real,n_prop)
c
	real avg_por(k), std_por(k), g(m), h(m),
     1int_ative(n_prop*n_active_blocks+1)
	integer NF
	character*80 DIR, ESTCOMPLX
	
		do ll=1, n_prop*n_l
				jpor= ll
				por_min=1.0E+38
				por_max=-1.0E+38
				sum_por=0.0
			if(active_blocks(ll).EQ.1) then
				do kkk=1,k
					if(por_min.GT.x(kkk,jpor)) por_min=x(kkk,jpor)
					if(por_max.LT.x(kkk,jpor)) por_max=x(kkk,jpor)
					sum_por=sum_por+x(int_active(kkk),jpor)
				enddo
				avg_por(jpor) = sum_por/k
				g(jpor)=por_min
				h(jpor)=por_max
				sum_std=0.0
				do kkk=1,k
					sum_std = sum_std + (avg_por(jpor)-x(kkk,jpor))**2
				enddo
				std_por(jpor)=sqrt(sum_std/(k-1))
			endif
			
	enddo
	open(21,file=DIR(:NF)//ESTCOMPLX,status='unknown')
		do jjj=1, k
		write(21,1090) jjj, avg_por(jjj), std_por(jjj), g(jjj),
     .h(jjj), x_real(jjj)
	enddo
	close(21)
1090	format(i5,1x,19(f8.4,2x))

	return
	end