PROGRAM plenum_chamber_simple
	IMPLICIT NONE
	REAL ::cross_sectional_a_first,cross_sectional_a_final !形状のBC
	REAL ::p_first,p_final !圧力のBC
	REAL ::cross_sectional_a(1001),v(1000),p(1001),mass_flow
	REAL ::initial_cond_v(1000),initial_cond_p(1001)
	REAL ::sudo_cond_v(1000),sudo_cond_p(1001),sudo_cond_mass_flow
	REAL ::sudo_v(1000),sudo_p(1001),sudo_mass_flow,param_d(1000),correction_p(1001)
	REAL ::density,relaxation_factor
	REAL ::ave_a,ave_a_e,ave_a_w,temp_real
	REAL ::temp_matrix(1000,1000),matrix_p(1000,1000),temp_vector(1000)!本当は９９８でOK
	REAL ::res_v(1000),first_residual_error,residual_error,allowable_value
	INTEGER :: mesh_num,max_number_of_iterations
	INTEGER :: i,j,k,l

	mesh_num = shellset_mesh_num
	cross_sectional_a_first = 0.5
	cross_sectional_a_final = 0.1
	p_first = 100
	p_final = 0
	density = 1.0
	relaxation_factor = 0.02d0
	max_number_of_iterations = 200000
	allowable_value = 0.1**5

	OPEN (1, file='output.dat', status='replace')
	WRITE (1, *) 'Mesh Num',mesh_num
	WRITE (1, *) 'Relaxation Factor',relaxation_factor
	WRITE (1, *) 'Allowable Value',allowable_value
	!PRINT *,'Mesh Num',mesh_num
	!PRINT *,'Relaxation Factor',relaxation_factor
	!PRINT *,'Allowable Value',allowable_value

	!断面積は線形に変化すると仮定(いちいち形状をインプットするのが面倒なため)
	DO i = 1, mesh_num + 1
	cross_sectional_a(i) = (cross_sectional_a_final - cross_sectional_a_first)*(i - 1)/mesh_num + cross_sectional_a_first
	END DO
!-------------------------------------initial cond---------------------------------------------------
	mass_flow = 1

	!流速の初期条件
	DO i = 1, mesh_num
	initial_cond_v(i) =  mass_flow/(density*(cross_sectional_a(i)+cross_sectional_a(i + 1))/2)
	END DO

	!圧力の初期条件(こっちも線形で繋ぐ)
	DO i = 1, mesh_num + 1
	initial_cond_p(i) =  (p_final - p_first)*(i - 1)/mesh_num + p_first
	END DO
!-------------------------------------SIMPLE algorithm---------------------------------------------------
	sudo_cond_mass_flow = mass_flow
	DO i = 1, mesh_num
	sudo_cond_v(i) =  initial_cond_v(i)
	END DO
	DO i = 1, mesh_num + 1
	sudo_cond_p(i) =  initial_cond_p(i)
	END DO

!-------------------------------------SIMPLE core---------------------------------------------------------
DO l = 1,max_number_of_iterations
	!偽速度算出(風上差分)
	i = 1
		ave_a = (cross_sectional_a(i + 1) + cross_sectional_a(i)) / 2
		sudo_v(i) = (sudo_cond_p(i) - sudo_cond_p(i + 1))*ave_a + density*(sudo_cond_v(i)**2)*(ave_a**2)/cross_sectional_a(i)
		temp_real = density*(sudo_cond_v(i)+sudo_cond_v(i+1)) / 2*cross_sectional_a(i + 1)
		temp_real = temp_real + density * sudo_cond_v(i)*(ave_a**3)/(cross_sectional_a(i)**2)/2
		sudo_v(i) = sudo_v(i) / temp_real
		param_d(i) = ave_a / temp_real
		IF(sudo_v(i)<0) THEN
		PRINT *,'ERROR!!!!!!!!!!!!!!!!!!',l,sudo_v(i)
		END IF
	DO i = 2, mesh_num -1
		ave_a = (cross_sectional_a(i + 1) + cross_sectional_a(i)) / 2
		sudo_v(i) = density * (sudo_cond_v(i - 1) + sudo_cond_v(i)) / 2 * cross_sectional_a(i) * sudo_v(i - 1)
		sudo_v(i) = sudo_v(i) + (sudo_cond_p(i) - sudo_cond_p(i + 1)) * ave_a
		temp_real = 0
		IF(density * (sudo_cond_v(i) + sudo_cond_v(i + 1)) / 2 * cross_sectional_a(i + 1) > 0) THEN
		temp_real = density * (sudo_cond_v(i) + sudo_cond_v(i + 1)) / 2 * cross_sectional_a(i + 1)
		END IF
		IF(density * (sudo_cond_v(i - 1) + sudo_cond_v(i)) / 2 * cross_sectional_a(i) < 0) THEN
		temp_real = temp_real - density * (sudo_cond_v(i - 1) + sudo_cond_v(i)) / 2 * cross_sectional_a(i)
		END IF
		sudo_v(i) = sudo_v(i) / temp_real
		param_d(i) = ave_a / temp_real
		IF(sudo_v(i)<0) THEN
		PRINT *,'ERROR!!!!!!!!!!!!!!!!!!',l,sudo_v(i)
		END IF
	END DO
	i = mesh_num
		ave_a = (cross_sectional_a(i + 1) + cross_sectional_a(i)) / 2
		sudo_v(i) = density * (sudo_cond_v(i - 1) + sudo_cond_v(i)) / 2 * cross_sectional_a(i) * sudo_v(i - 1)
		sudo_v(i) = sudo_v(i) + (sudo_cond_p(i) - sudo_cond_p(i + 1)) * ave_a
		temp_real = density*sudo_cond_v(i)*ave_a
		sudo_v(i) = sudo_v(i) / temp_real
		param_d(i) = ave_a / temp_real
		IF(sudo_v(i)<0) THEN
		PRINT *,'ERROR!!!!!!!!!!!!!!!!!!',l,sudo_v(i)
		END IF

	!偽圧力算出matrix(i,j)
	DO i = 2, mesh_num
		ave_a_e = (cross_sectional_a(i - 1) + cross_sectional_a(i)) / 2
		ave_a_w = (cross_sectional_a(i + 1) + cross_sectional_a(i)) / 2
		!行列計算がややこしいので、ここで１ずらす。
		matrix_p(i - 1,i - 1) = density * param_d(i - 1) * ave_a_e + density * param_d(i) * ave_a_w
		IF (i < mesh_num) THEN
			matrix_p(i - 1,i) = (-1)*density * param_d(i) * ave_a_w
		END IF
		IF (i > 2) THEN
			matrix_p(i - 1,i - 2) = (-1)*density * param_d(i - 1) * ave_a_e
		END IF
	END DO
	!気合の掃き出し法(本当はここも反復法を使うべきだが、圧力の補正には対角優位が保証されるのでこれでもかまわないはず)
	DO i = 1, mesh_num - 1
		DO j = 1, mesh_num - 1
				temp_matrix(i,j) = 0
		END DO
		temp_matrix(i,i) = 1
	END DO
	DO i=1, mesh_num - 1
		temp_real = 1 / matrix_p(i,i)
		DO j = 1,mesh_num - 1
			matrix_p(i,j) = matrix_p(i,j) * temp_real
			temp_matrix(i,j) = temp_matrix(i,j) * temp_real
		END DO
		DO j = 1, mesh_num - 1
	 		IF (i /= j) THEN
				temp_real = matrix_p(j,i)
				DO k = 1, mesh_num - 1
					matrix_p(j,k) = matrix_p(j,k) - matrix_p(i,k)*temp_real
					temp_matrix(j,k) = temp_matrix(j,k) - temp_matrix(i,k)*temp_real
				END DO
			END IF
		END DO
	END DO
	DO i = 1,mesh_num - 1
		ave_a_e = (cross_sectional_a(i) + cross_sectional_a(i + 1)) / 2
		ave_a_w = (cross_sectional_a(i + 1) + cross_sectional_a(i + 2)) / 2
		temp_vector(i) = density * sudo_v(i) * ave_a_e - density * sudo_v(i + 1) * ave_a_w
	END DO
	DO i = 1,mesh_num - 1
		correction_p(i + 1) = 0
		DO j = 1,mesh_num - 1
			!１ずらしたのを、ここでつじつまを合わせる。
			correction_p(i + 1) = correction_p(i + 1) + temp_matrix(i,j) * temp_vector(j)
		END DO
	END DO
	DO i = 1,mesh_num + 1
		sudo_p(i) = sudo_cond_p(i) + correction_p(i)
	END DO

	!速度と圧力の補正を行う。面倒なので偽速度と偽圧力に代入
	sudo_mass_flow = 0
	DO i = 1,mesh_num
		sudo_v(i) = sudo_v(i) + param_d(i) * (correction_p(i) - correction_p(i + 1))
		ave_a = (cross_sectional_a(i + 1) + cross_sectional_a(i)) / 2
		sudo_mass_flow = sudo_mass_flow + density * sudo_v(i) *ave_a
	END DO
	sudo_mass_flow = sudo_mass_flow / mesh_num
	i = 1!偽圧力の補正は特別にこれだけ行う。
		ave_a = (cross_sectional_a(i + 1) + cross_sectional_a(i)) / 2
		sudo_p(i) = p_first - density * ((sudo_v(i) * ave_a / cross_sectional_a(i))**2) / 2
	!不足緩和
	DO i = 1,mesh_num
		sudo_cond_v(i) = (1 - relaxation_factor) * sudo_cond_v(i) + relaxation_factor * sudo_v(i)
	END DO
	DO i = 1,mesh_num + 1
		sudo_cond_p(i) = (1 - relaxation_factor) * sudo_cond_p(i) + relaxation_factor * sudo_p(i)
	END DO
	sudo_cond_mass_flow = (1 - relaxation_factor) * sudo_cond_mass_flow + relaxation_factor * sudo_mass_flow

	!残差を算出
	i = 1
	ave_a = (cross_sectional_a(i + 1) + cross_sectional_a(i)) / 2
	res_v(i) = (sudo_cond_p(i) - sudo_cond_p(i + 1))*ave_a + density*(sudo_cond_v(i)**2)*(ave_a**2)/cross_sectional_a(i)
	temp_real = density*(sudo_cond_v(i)+sudo_cond_v(i+1)) / 2*cross_sectional_a(i + 1)
	temp_real = temp_real + density * sudo_cond_v(i)*(ave_a**3)/(cross_sectional_a(i)**2)/2
	res_v(i) = temp_real * sudo_cond_v(i) - res_v(i)
	residual_error = ABS(res_v(i))
	DO i = 2, mesh_num -1
		ave_a = (cross_sectional_a(i + 1) + cross_sectional_a(i)) / 2
		res_v(i) = density * (sudo_cond_v(i - 1) + sudo_cond_v(i)) / 2 * cross_sectional_a(i) * sudo_v(i - 1)
		res_v(i) = res_v(i) + (sudo_cond_p(i) - sudo_cond_p(i + 1)) * ave_a
		temp_real = 0
		IF(density * (sudo_cond_v(i) + sudo_cond_v(i + 1)) / 2 * cross_sectional_a(i + 1) > 0) THEN
		temp_real = density * (sudo_cond_v(i) + sudo_cond_v(i + 1)) / 2 * cross_sectional_a(i + 1)
		END IF
		IF(density * (sudo_cond_v(i - 1) + sudo_cond_v(i)) / 2 * cross_sectional_a(i) < 0) THEN
		temp_real = temp_real - density * (sudo_cond_v(i - 1) + sudo_cond_v(i)) / 2 * cross_sectional_a(i)
		END IF
		res_v(i) = temp_real * sudo_cond_v(i) - res_v(i)
		residual_error = residual_error + ABS(res_v(i))
	END DO
	i = mesh_num
		ave_a = (cross_sectional_a(i + 1) + cross_sectional_a(i)) / 2
		res_v(i) = density * (sudo_cond_v(i - 1) + sudo_cond_v(i)) / 2 * cross_sectional_a(i) * sudo_v(i - 1)
		res_v(i) = res_v(i) + (sudo_cond_p(i) - sudo_cond_p(i + 1)) * ave_a
		temp_real = density*sudo_cond_v(i)*ave_a
		res_v(i) = temp_real * sudo_cond_v(i) - res_v(i)
		residual_error = residual_error + ABS(res_v(i))
	IF (l == 1) THEN!相対残差を算出する
		first_residual_error = residual_error
	END IF
	WRITE (1,*) l,'preResidual error',residual_error
	residual_error = residual_error / first_residual_error
	WRITE (1,*) l,'Residual error',residual_error,'Mass Flow',sudo_cond_mass_flow
	!PRINT *,l,'Residual error',residual_error,'Mass Flow',sudo_cond_mass_flow
	IF (residual_error < allowable_value .or. isnan(sudo_cond_mass_flow)) THEN
		!PRINT *,l,'Residual error',residual_error,'Mass Flow',sudo_cond_mass_flow
		EXIT
	END IF
END DO
CLOSE(1)
temp_real = SQRT(2*(p_first - p_final) / density) * cross_sectional_a_final
!PRINT *,'理論解(Mass Flow)',temp_real
!PRINT *,'----------------------------------------------------------------------------'
!計算講師の数、緩和係数、許容残差、反復回数、残差、流量、理論解の流量
PRINT *,mesh_num,relaxation_factor,allowable_value,l,residual_error,sudo_cond_mass_flow,temp_real
END PROGRAM plenum_chamber_simple