PROGRAM each_kakezan
	IMPLICIT NONE
	INTEGER(8) :: number1,number2,kake,temp_num,temp_num1,temp_num2
	INTEGER :: num(100),temp(100)
	INTEGER :: i,j,k,l,m
	OPEN(1, file='output.dat', status='replace')
	OPEN(1, file='output.dat', position='append')
	
	DO number1 = 999999991,999999999
		DO number2 = 999999991,999999999
			temp_num1 = number1
			temp_num2 = number2
			DO i = 2,20
				temp_num = kake(temp_num1) * kake(temp_num2)
				IF ( i > 10) THEN
					PRINT *,number1,number2,i,temp_num
				END IF
				IF (temp_num < 10) THEN
					EXIT
				END IF
			END DO
		write (1,*) number1,number2,i,temp_num
		END DO
	END DO
	close(1)
END PROGRAM each_kakezan

INTEGER(8) FUNCTION kake( n )
	IMPLICIT NONE
	INTEGER(8) :: n,temp_n
	INTEGER :: num(100)
	INTEGER :: i
	IF( n==0 ) THEN
		kake = 1
	ELSE
		num(1) = MOD(n,10)
		temp_n = num(1)
		DO i = 2,100
			IF ( INT(n/10**(i-1)) == 0 ) THEN
				EXIT
			END IF
		num(i) = INT(MOD(n,10**i)/10**(i-1))
		temp_n = temp_n * num(i)
		END DO
		kake = temp_n
	END IF
END FUNCTION kake