Module WatLog
    Implicit None
    
    Character(Len=12) :: OUTWL = 'WatLog.OUT'
    Integer :: NOUTWL
    Real, dimension(20) :: fdsw_test
    
    Real, Dimension(:), Allocatable :: Perched_Water_Top_By_Day
    Integer :: WaterLoggingTime, g_nrlayr
    Real :: grtdep_nw_before_kill
    Real :: gkill_depth
    
    Integer, Dimension(10) :: WatLogTime
    Real, Dimension(4) :: Perched_Water_Top_Daily
    Real, Dimension(3) :: Perched_Water_Top_Daily1
    Integer :: WatLoggCount, sz
    Real :: rtdep_old
Contains
!-------------------------------------------------------------------------------   
!   Calculates Fraction of Drainable Soil Water (0-1) for each soil layer
!   Returns as an array
    Function FDW(&
        NLAYR, DUL, SAT, SW)                  !Input
    
        Integer :: NLAYR
        Real, Dimension(NLAYR) :: DUL, SAT, SW
        Real, Dimension(NLAYR) :: FDW
        
        Integer :: L
        Real, Dimension(NLAYR) :: SATFRAC
        
        Do L=1, NLAYR
            If (SW(L) - DUL(L) > 0.0) Then
                SATFRAC(L) = (SW(L) - DUL(L)) / (SAT(L) - DUL(L))
                SATFRAC(L) = MIN(SATFRAC(L), 1.0)
            Else
                SATFRAC(L) = 0.0
            End If       
        End Do
        FDW = SATFRAC
    End Function FDW    
!-------------------------------------------------------------------------------         
!   Calculates if layer is saturated: Fraction of Drainable Soil Water is
!   more than some limit value        
!   Returns as an array of 0s - not and 1s - saturated
    Function SatLayers( &
        NLAYR, FDSW, TOL)
    
        Integer :: NLAYR
        Real, Dimension(NLAYR) :: FDSW
        Integer, Dimension(NLAYR) :: SatLayers
        Real :: TOL
        
        Integer, Dimension(NLAYR) :: Sat_Layer
        Integer :: L
        
        Do L=1, NLAYR
            If (FDSW(L) > TOL) Then
                Sat_Layer(L) = 1
            Else
                Sat_Layer(L) = 0
            End If       
        End Do
        SatLayers = Sat_Layer
    End Function
!-------------------------------------------------------------------------------        
        
    Subroutine WTDEPT2( &
        NLAYR, Layer_Thikness, Layer_Bottom, DUL, SAT, SW, &                 !Input
!        WTDEP, WT_Thikness)
        Perched_Water_Top)
    
        Integer :: NLAYR
        Real, Dimension(NLAYR) :: Layer_Thikness, Layer_Bottom, DUL, SAT, SW
        
        Real :: Perched_Water_Top
        Real :: FACTOR             
        Real, Dimension(NLAYR) :: SATFRAC, WTDEP, WT_Thikness 
        Integer, Dimension(NLAYR) :: Sat_Layer 
        Logical :: isSatLayer
        
        Real, Parameter :: TOL = 0.95      
        Integer :: L, i, j, not_sat_layr
        
        SATFRAC = FDW (NLAYR, DUL, SAT, SW)
        
        Sat_Layer = SatLayers(NLAYR, SATFRAC, TOL)
        
        isSatLayer = .FALSE.
        
        Do L=1, NLAYR
            If (Sat_Layer(L) == 1) Then
                If (L == 1) Then
                   Perched_Water_Top = 0
                Else
                   !Perched_Water_Top = Layer_Bottom(L-1)
                   !If (SATFRAC(L-1) > 0) then
                   !   Perched_Water_Top = Perched_Water_Top - Layer_Thikness(L-1) * SATFRAC(L-1)
                   !End If
                   FACTOR = MIN(MAX(0.0, (SATFRAC(L) - TOL) / (1.0 - TOL)),1.0)
                   Perched_Water_Top = Layer_Bottom(L-1) - Layer_Thikness(L-1) * SATFRAC(L-1) * FACTOR
                End If
                isSatLayer = .TRUE.
                Exit
            End If
            Perched_Water_Top = Layer_Bottom(NLAYR)
        End Do
            
        !WTDEP = 0. 
        !WT_Thikness = 0.
        !if (isSatLayer) then
        !    L=NLAYR
        !    j=1
        !    Do
        !       WT_Thikness(j) = 0.
        !       if (Sat_Layer(L) == 1) then
        !            WTDEP(j) = Layer_Bottom(L)
        !            WT_Thikness(j) = Layer_Thikness(L)
        !            do i = L-1, 1, -1
        !               if (Sat_Layer(i) == 1) then   
        !                  WT_Thikness(j) = WT_Thikness(j) + Layer_Thikness(i)
        !                  not_sat_layr = i-1
        !               else
        !                  not_sat_layr = i
        !                  Exit
        !               end If
        !            End do
        !            j=j+1
        !            if (not_sat_layr > 0) then
        !                L = not_sat_layr
        !            else
        !                L = 1
        !            End if
        !       End If
        !   
        !       L=L-1
        !       if (L == 0) Exit
        !    End Do
        !End if
    End Subroutine WTDEPT2 
End Module WatLog
 