PROGRAM tenpuzzle
	IMPLICIT NONE
	REAL ::realtemp,randomtemp
	INTEGER :: num(4),all_num_length,number
	INTEGER :: kari(4),temp(4),flag(4)
	INTEGER :: i,j,k,l,m
	CHARACTER :: formula,formulatemp
	OPEN(17, file='output.dat', status='replace')
	
	DO number = 0000,9999
	num(1) = INT(number/1000)
	num(2)= INT(number/100) - num(1)*10
	num(3)= INT(number/10) - num(1)*100 - num(2)*10
	num(4)= MOD(number,10)
	OPEN(17, file='output.dat', position='append')
	all_num_length = 4
	DO i = 1, 200000
		DO j = 1, all_num_length
			kari(j)=num(j)
		END DO
		l = 1
		DO j = 1, all_num_length
			CALL RANDOM_NUMBER(randomtemp)
			k = (all_num_length - j + 1)*randomtemp + 1
			temp(l) = kari(k)
			DO m = 1, all_num_length
				IF (m>k-1) THEN
				kari(m) = kari(m + 1)
				END IF
			END DO
			l = l + 1
		END DO
		realtemp = temp(1)
		DO j = 1, all_num_length - 1
			CALL RANDOM_NUMBER(randomtemp)
			flag(j) = 4*randomtemp + 1
			SELECT CASE (flag(j))
				CASE (1)
					realtemp = realtemp + temp(j+1)
				CASE (2)
					realtemp = realtemp - temp(j+1)
				CASE (3)
					realtemp = realtemp * temp(j+1)
				CASE (4)
					realtemp = realtemp / temp(j+1)
				CASE DEFAULT
					PRINT *,'ERROR'
			END SELECT
		END DO
		IF ( realtemp < 10.001 .and. realtemp > 9.999) THEN
			PRINT *,number,temp(1),flag(1),temp(2),flag(2),temp(3),flag(3),temp(4),realtemp
			PRINT *,'10find'
			write (17,*) number,temp(1),flag(1),temp(2),flag(2),temp(3),flag(3),temp(4),realtemp
			EXIT
		END IF
		IF ( realtemp > -10.001 .and. realtemp < -9.999) THEN
			PRINT *,number,temp(1),flag(1),temp(2),flag(2),temp(3),flag(3),temp(4),realtemp
			PRINT *,'10find'
			write (17,*) number,temp(1),flag(1),temp(2),flag(2),temp(3),flag(3),temp(4),realtemp
			EXIT
		END IF
		IF (flag(1)==4 .or. flag(2)==4 .or. flag(3)==4 .or. flag(4)==4) THEN
			realtemp = 1/realtemp
			IF ( realtemp < 10.001 .and. realtemp > 9.999) THEN
				PRINT *,number,temp(1),flag(1),temp(2),flag(2),temp(3),flag(3),temp(4),realtemp
				PRINT *,'10find'
				write (17,*) number,temp(1),flag(1),temp(2),flag(2),temp(3),flag(3),temp(4),realtemp,'reverse'
				EXIT
			END IF
			IF ( realtemp > -10.001 .and. realtemp < -9.999) THEN
				PRINT *,number,temp(1),flag(1),temp(2),flag(2),temp(3),flag(3),temp(4),realtemp
				PRINT *,'10find'
				write (17,*) number,temp(1),flag(1),temp(2),flag(2),temp(3),flag(3),temp(4),realtemp,'reverse'
				EXIT
			END IF
		END IF
	END DO
	close(17)
	END DO
END PROGRAM tenpuzzle
