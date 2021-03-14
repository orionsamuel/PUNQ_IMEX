	real tempo_wcut(83), wcut_obs(83,6),mwcut, varwcut
	real tempo_bhp(83), bhp_obs(83,6), mbhp, varbhp
	real tempo_gor(83), gor_obs(83,6), mgor, vargor
		
	character*10 yymmdd_wcut(83), yymmdd_bhp(83),
     .yymmdd_gor(83)
	
	integer Nmed_wcut, Nmed_bhp, Nmed_gor, Npocos
	
	Nmed_wcut=83
	Nmed_bhp=83
	Nmed_gor=83
	Npocos=6
	

	open(6,file='truth_model.dat',status='unknown')
		read(6,*)
		read(6,*)
		read(6,*)
		read(6,*)
			do ii=1,Nmed_wcut
				read(6,*) tempo_wcut(ii), yymmdd_wcut(ii),
     1			(wcut_obs(ii,kk),kk=1, Npocos)
			enddo
		read(6,*)
		read(6,*)
		read(6,*)
		read(6,*)
		read(6,*)
		read(6,*)
		read(6,*)
			do ii=1,Nmed_bhp
				read(6,*) tempo_bhp(ii), yymmdd_bhp(ii), 
	1			(bhp_obs(ii,kk),kk=1, Npocos)
			enddo
		read(6,*)
		read(6,*)
		read(6,*)
		read(6,*)
		read(6,*)
		read(6,*)
			do ii=1,Nmed_gor
				read(6,*) tempo_gor(ii), yymmdd_gor(ii),
	1			(gor_obs(ii,kk),kk=1, Npocos)
			enddo
	close(6)

c	CALCULO DAS VARIANCIAS
	
	mwcut=0.0
	mbhp=0.0
	mgor=0.0
	do ii=1, Nmed_wcut
		do kk=1, Npocos
			mwcut = mwcut + wcut_obs(ii,kk)
			mbhp = mbhp + bhp_obs(ii,kk)
			mgor = mgor + gor_obs(ii,kk)
		enddo
	enddo
			
			mwcut = mwcut/(Nmed_wcut*Npocos)
			mbhp = mbhp/(Nmed_bhp*Npocos)
			mgor = mgor/(Nmed_gor*Npocos)
			
			
	varwcut=0.0
	varbhp=0.0
	vargor=0.0
	
	do ii=1, Nmed_wcut
		do kk=1, Npocos
			varwcut = varwcut + (wcut_obs(ii,kk)-mwcut)**2
			varbhp = varbhp + (bhp_obs(ii,kk)-mbhp)**2
			vargor = vargor + (gor_obs(ii,kk)-mgor)**2
		enddo
	enddo
			varwcut = varwcut/(Nmed_wcut*Npocos)
			varbhp = varbhp/(Nmed_bhp*Npocos)
			vasgor = vargor/(Nmed_gor*Npocos)

	write(*,*) 'dp wct=', sqrt(varwcut)
	write(*,*) 'dp bhp=', sqrt(varbhp)
	write(*,*) 'dp gor=', sqrt(vargor)

	stop
	end