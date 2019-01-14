module module_anime

use communication_helper_mpi
contains

!subroutine anime(n,n0,nmax,km,jm,im,dxl,dx1,dyl,dy1,z2,data22,data23,u,w,v,amask1,zbm1)
subroutine anime(n,n0,nmax,km,jm,im,dxl,dx1,dyl,dy1,z2,data22,data23,u,w,v,amask1,zbm)

    use common_sn ! create_new_include_statements() line 102
    real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1) , intent(InOut) :: amask1
    character(len=70), intent(In) :: data22
    character(len=70), intent(In) :: data23
    real(kind=4), dimension(-1:ip+1) , intent(In) :: dx1
    real(kind=4), dimension(0:ip) , intent(In) :: dxl
    real(kind=4), dimension(0:jp+1) , intent(In) :: dy1
    real(kind=4), dimension(0:jp) , intent(In) :: dyl
    integer, intent(In) :: im
    integer, intent(In) :: jm
    integer, intent(In) :: km
    integer, intent(In) :: n
    integer, intent(In) :: n0
    integer, intent(In) :: nmax
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: u
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: v
    real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(In) :: w
    real(kind=4), dimension(kp+2) , intent(In) :: z2
    real(kind=4), dimension(-1:ipmax+1,-1:jpmax+1) , intent(In)  :: zbm
!average_out
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1)  :: uani
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1)  :: vani
    real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) :: wani
!mpi_out

    real(kind=4),allocatable :: ua(:,:,:)
    real(kind=4),allocatable :: va(:,:,:)
    real(kind=4),allocatable :: wa(:,:,:)
    real(kind=4),allocatable :: amask1a(:,:,:)




    if(n == n0.or.n == nmax.or.mod(n,1000) == 0.) then
        do k = 1,km
            do j = 1,jm
                do i = 1,im
                    a1(i,j,k) = real(dxl(i-1)+dx1(i))
                    a2(i,j,k) = real(dyl(j-1)+dy1(j))
                    a3(i,j,k) = real(z2(k))
                end do
            end do
        end do
        open(unit=22,file=data22,form='unformatted',status='unknown')
        write(22) im,jm,km
        write(22) (((real(a1(i,j,k)),i=1,im),j=1,jm),k=1,km), &
                  (((real(a3(i,j,k)),i=1,im),j=1,jm),k=1,km), &
                  (((real(a2(i,j,k)),i=1,im),j=1,jm),k=1,km)
        close(unit=22)
   end if
#ifdef MPI






!for_average_out
      do k=0,km
      do j=0,jm
      do i=0,im
      uani(i,j,k)=uani(i,j,k)+u(i,j,k)
      vani(i,j,k)=vani(i,j,k)+v(i,j,k)
      wani(i,j,k)=wani(i,j,k)+w(i,j,k)
      end do
      end do
      end do

!reset
!      if(n.eq.40000) then
!      do k=0,km
!      do j=0,jm
!      do i=0,im
!      uani(i,j,k)=0.
!      vani(i,j,k)=0.
!      wani(i,j,k)=0.
!      end do
!      end do
!      end do
!      end if


!       if(n.gt.40001.and.mod(n,50).eq.0) then
       if(mod(n,50).eq.0) then ! for outputing every 50 timesteps


       if (isMaster()) then
       write(filename, '("../out/data23",i6.6, ".dat")') n
       open(unit=23,file=filename,form='unformatted',access='direct',recl=4*ipmax*jpmax) !for gfortran, recl=4*ipmax*jpmax
       end if

       allocate(ua(0:ipmax+1,-1:jpmax+1,0:kp+1))
        call distributeu(ua, uani, ip, jp, kp, ipmax, jpmax, procPerRow)
       if (isMaster()) then
          do k = 1,km
            do j = 1,jm
                do i = 1,im
                 ua(i,j,k) = uani(i,j,k)
                end do
            end do
          end do

       do k=1,km
        do j=1,jpmax
         do i=1,ipmax
            ua(i,j,k)=ua(i,j,k)/50.
         end do
        end do
       end do

!boundary
       do k = 1,km
         do j = 1,jpmax
            ua(0,j,k) = ua(1,j,k)
         end do
       end do

       irec = 1
       do  k=1,km
       write(23,rec=irec) ((real(0.5*(ua(i-1,j,k)+ua(i,j,k))),i=1,ipmax),j=1,jpmax)
       irec = irec + 1
       end do
       end if


       deallocate(ua)


       allocate(wa(0:ipmax+1,-1:jpmax+1,-1:kp+1))
        call distributew(wa, wani, ip, jp, kp, ipmax, jpmax, procPerRow)
       if (isMaster()) then
          do k = 1,km
            do j = 1,jm
                do i = 1,im
                 wa(i,j,k) = wani(i,j,k)
                end do
            end do
          end do

       do k=1,km
        do j=1,jpmax
         do i=1,ipmax
            wa(i,j,k)=wa(i,j,k)/50.
         end do
        end do
       end do

!boundary
            do j = 1,jpmax
                do i = 1,ipmax
                    wa(i,j,0) = 0.0
                end do
            end do

       do  k=1,km
       write(23,rec=irec) ((real(0.5*(wa(i,j,k-1)+wa(i,j,k))),i=1,ipmax),j=1,jpmax)
       irec = irec + 1
       end do
       end if
       deallocate(wa)


       allocate(va(0:ipmax+1,-1:jpmax+1,0:kp+1))
        call distributev(va, vani, ip, jp, kp, ipmax, jpmax, procPerRow)
       if (isMaster()) then
          do k = 1,km
            do j = 1,jm
                do i = 1,im
                 va(i,j,k) = vani(i,j,k)
                end do
            end do
          end do

       do k=1,km
        do j=1,jpmax
         do i=1,ipmax
            va(i,j,k)=va(i,j,k)/50.
         end do
        end do
       end do

!boundary
            do k = 1,km
                do i = 1,ipmax
                    va(i,0,k) = va(i,jpmax,k)
                end do
            end do


       do  k=1,km
       write(23,rec=irec) ((real(0.5*(va(i,j-1,k)+va(i,j,k))),i=1,ipmax),j=1,jpmax)
       irec = irec + 1
       end do
       end if
       deallocate(va)


       allocate(amask1a(0:ipmax+1,0:jpmax+1,0:kp+1))
        call distributeamask(amask1a, amask1, ip, jp, kp, ipmax, jpmax, procPerRow)
       if (isMaster()) then
          do k = 1,km
            do j = 1,jm
                do i = 1,im
                 amask1a(i,j,k) = amask1(i,j,k)
                end do
            end do
          end do




       do  k=1,km
       write(23,rec=irec) ((real(amask1a(i,j,k)),i=1,ipmax),j=1,jpmax)
       irec = irec + 1
       end do
       end if
       deallocate(amask1a)


       if (isMaster()) then
       write(23,rec=irec) ((real(zbm(i,j)),i=1,ipmax),j=1,jpmax)
       irec= irec + 1


       close(unit=23)
       end if

      do k=0,km
      do j=0,jm
      do i=0,im
      uani(i,j,k)=0.
      vani(i,j,k)=0.
      wani(i,j,k)=0.
      end do
      end do
      end do

      end if

#endif

end subroutine anime



subroutine anime_bond(n,n0,nmax,km,jm,im,dxl,dx1,dyl,dy1,z2,data22,data23,u,w,v,amask1,zbm)

    use common_sn ! create_new_include_statements() line 102
    real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1) , intent(InOut) :: amask1
    character(len=70), intent(In) :: data22
    character(len=70), intent(In) :: data23
    real(kind=4), dimension(-1:ip+1) , intent(In) :: dx1
    real(kind=4), dimension(0:ip) , intent(In) :: dxl
    real(kind=4), dimension(0:jp+1) , intent(In) :: dy1
    real(kind=4), dimension(0:jp) , intent(In) :: dyl
    integer, intent(In) :: im
    integer, intent(In) :: jm
    integer, intent(In) :: km
    integer, intent(In) :: n
    integer, intent(In) :: n0
    integer, intent(In) :: nmax
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: u
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: v
    real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(In) :: w
    real(kind=4), dimension(kp+2) , intent(In) :: z2
    real(kind=4), dimension(-1:ipmax+1,-1:jpmax+1) , intent(In)  :: zbm

!bond_out
    real(kind=4), dimension(1,jpmax,kp)  :: ubonda
    real(kind=4), dimension(1,jpmax,kp)  :: vbonda
    real(kind=4), dimension(1,jpmax,kp)  :: wbonda


#ifdef MPI



       call distributebondoutu(ubonda, u, ip, jp, kp, ipmax, jpmax, procPerRow)
       call distributebondoutu(vbonda, v, ip, jp, kp, ipmax, jpmax, procPerRow)
       call distributebondoutw(wbonda, w, ip, jp, kp, ipmax, jpmax, procPerRow)



       if (isMaster()) then


       write(filename, '("data24",i6.6, ".dat")') n

       open(unit=24,file=filename,form='unformatted',status='replace',access='direct',recl=4*jpmax)
       irec = 1
        do k=1,km
        write(24,rec=irec) (ubonda(1,j,k),j=1,jpmax)
      irec=irec+1
        end do

        do k=1,km
        write(24,rec=irec) (wbonda(1,j,k),j=1,jpmax)
      irec=irec+1
        end do

        do k=1,km
        write(24,rec=irec) (vbonda(1,j,k),j=1,jpmax)
      irec=irec+1
        end do

       close(unit=24)


       end if
#endif

end subroutine anime_bond




!data30,31
subroutine ifdata_out(n,n0,n1,nmax,time,km,jm,im,u,w,v,p,usum,vsum,wsum,f,g,h,fold,gold,hold)


    use common_sn ! create_new_include_statements() line 102
    integer, intent(In) :: im
    integer, intent(In) :: jm
    integer, intent(In) :: km
    integer, intent(In) :: n
    integer, intent(In) :: n0
    integer, intent(In) :: nmax
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: u
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: v
    real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(In) :: w
    integer, intent(In) :: n1
    real, intent(In) :: time
!ifdata
    real(kind=4), dimension(0:ip,0:jp,0:kp), intent(In)  :: usum
    real(kind=4), dimension(0:ip,0:jp,0:kp), intent(In)  :: vsum
    real(kind=4), dimension(0:ip,0:jp,0:kp), intent(In)  :: wsum
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+1),intent(In)  :: p

    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In)  :: f
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In)  :: g
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(In)  :: h

    real(kind=4), dimension(ip,jp,kp) , intent(In)  :: fold
    real(kind=4), dimension(ip,jp,kp) , intent(In)  :: gold
    real(kind=4), dimension(ip,jp,kp) , intent(In)  :: hold

!mpi_out
    real(kind=4),allocatable :: ua(:,:,:)
    real(kind=4),allocatable :: va(:,:,:)
    real(kind=4),allocatable :: wa(:,:,:)
    real(kind=4),allocatable :: usuma(:,:,:)
    real(kind=4),allocatable :: vsuma(:,:,:)
    real(kind=4),allocatable :: wsuma(:,:,:)
    real(kind=4),allocatable :: pa(:,:,:)
    real(kind=4),allocatable :: fa(:,:,:)
    real(kind=4),allocatable :: ga(:,:,:)
    real(kind=4),allocatable :: ha(:,:,:)
    real(kind=4),allocatable :: folda(:,:,:)
    real(kind=4),allocatable :: golda(:,:,:)
    real(kind=4),allocatable :: holda(:,:,:)



       if((n.eq.n1-1).or.(n.eq.nmax))  then      

        if (isMaster()) then
        write(filename, '("data30",i6.6, ".dat")') n

        open(unit=30,file=filename,form='unformatted',status='replace')

        write(30) n,time
        end if

       allocate(ua(0:ipmax+1,-1:jpmax+1,0:kp+1))
        call distributeu(ua, u, ip, jp, kp, ipmax, jpmax, procPerRow)
       if (isMaster()) then
          do k = 1,km
            do j = 1,jm
                do i = 1,im
                    ua(i,j,k) = u(i,j,k)
                end do
            end do
          end do
        write(30) (((ua(i,j,k),i=1,ipmax),j=1,jpmax),k=1,km)
        end if
        deallocate(ua)


       allocate(va(0:ipmax+1,-1:jpmax+1,0:kp+1))
        call distributev(va, v, ip, jp, kp, ipmax, jpmax, procPerRow)
       if (isMaster()) then
          do k = 1,km
            do j = 1,jm
                do i = 1,im 
                    va(i,j,k) = v(i,j,k)
                end do
            end do
          end do
        write(30) (((va(i,j,k),i=1,ipmax),j=1,jpmax),k=1,km)
        end if
        deallocate(va)


       allocate(wa(0:ipmax+1,-1:jpmax+1,-1:kp+1))
        call distributew(wa, w, ip, jp, kp, ipmax, jpmax, procPerRow)
       if (isMaster()) then
          do k = 1,km
            do j = 1,jm
                do i = 1,im 
                    wa(i,j,k) = w(i,j,k)
                end do
            end do
          end do
        write(30) (((wa(i,j,k),i=1,ipmax),j=1,jpmax),k=1,km)
        end if
        deallocate(wa)


       allocate(pa(0:ipmax+2,0:jpmax+2,0:kp+1))
        call distributep(pa, p, ip, jp, kp, ipmax, jpmax, procPerRow)
       if (isMaster()) then
          do k = 1,km
            do j = 1,jm
                do i = 1,im 
                    pa(i,j,k) = p(i,j,k)
                end do
            end do
          end do
        write(30) (((pa(i,j,k),i=1,ipmax),j=1,jpmax),k=1,km)
        end if
        deallocate(pa)


       allocate(usuma(0:ipmax,0:jpmax,0:kp))
        call distributeusum(usuma, usum, ip, jp, kp, ipmax, jpmax, procPerRow)
       if (isMaster()) then
          do k = 1,km
            do j = 1,jm
                do i = 1,im 
                    usuma(i,j,k) = usum(i,j,k)
                end do
            end do
          end do
        write(30) (((usuma(i,j,k),i=1,ipmax),j=1,jpmax),k=1,km)
        end if
        deallocate(usuma)


      allocate(vsuma(0:ipmax,0:jpmax,0:kp))
        call distributeusum(vsuma, vsum, ip, jp, kp, ipmax, jpmax, procPerRow)
       if (isMaster()) then
          do k = 1,km
            do j = 1,jm
                do i = 1,im
                    vsuma(i,j,k) = vsum(i,j,k)
                end do
            end do
          end do
        write(30) (((vsuma(i,j,k),i=1,ipmax),j=1,jpmax),k=1,km)
        end if
        deallocate(vsuma)


     allocate(wsuma(0:ipmax,0:jpmax,0:kp))
        call distributeusum(wsuma, wsum, ip, jp, kp, ipmax, jpmax, procPerRow)
       if (isMaster()) then
          do k = 1,km
            do j = 1,jm
                do i = 1,im
                    wsuma(i,j,k) = wsum(i,j,k)
                end do
            end do
          end do
        write(30) (((wsuma(i,j,k),i=1,ipmax),j=1,jpmax),k=1,km)
        close(30)
 
        end if
        deallocate(wsuma)




        if (isMaster()) then
        write(filename, '("data31",i6.6, ".dat")') n
        open(unit=31,file=filename,form='unformatted',status='replace')

        end if


     allocate(fa(0:ipmax,0:jpmax,0:kp))
        call distributef(fa, f, ip, jp, kp, ipmax, jpmax, procPerRow)
       if (isMaster()) then
          do k = 1,km
            do j = 1,jm
                do i = 1,im
                    fa(i,j,k) = f(i,j,k)
                end do
            end do
          end do
        write(31) (((fa(i,j,k),i=1,ipmax),j=1,jpmax),k=1,km)
        end if
        deallocate(fa)


     allocate(ga(0:ipmax,0:jpmax,0:kp))
        call distributef(ga, g, ip, jp, kp, ipmax, jpmax, procPerRow)
       if (isMaster()) then
          do k = 1,km
            do j = 1,jm
                do i = 1,im
                    ga(i,j,k) = g(i,j,k)
                end do
            end do
          end do
        write(31) (((ga(i,j,k),i=1,ipmax),j=1,jpmax),k=1,km)
        end if
        deallocate(ga)


     allocate(ha(0:ipmax,0:jpmax,0:kp))
        call distributef(ha, h, ip, jp, kp, ipmax, jpmax, procPerRow)
       if (isMaster()) then
          do k = 1,km
            do j = 1,jm
                do i = 1,im
                    ha(i,j,k) = h(i,j,k)
                end do
            end do
          end do
        write(31) (((ha(i,j,k),i=1,ipmax),j=1,jpmax),k=1,km)
        end if
        deallocate(ha)


     allocate(folda(ipmax,jpmax,kp))
        call distributefold(folda, fold, ip, jp, kp, ipmax, jpmax, procPerRow)
       if (isMaster()) then
          do k = 1,km
            do j = 1,jm
                do i = 1,im
                    folda(i,j,k) = fold(i,j,k)
                end do
            end do
          end do
        write(31) (((folda(i,j,k),i=1,ipmax),j=1,jpmax),k=1,km)
        end if
        deallocate(folda)


     allocate(golda(ipmax,jpmax,kp))
        call distributefold(golda, gold, ip, jp, kp, ipmax, jpmax, procPerRow)
       if (isMaster()) then
          do k = 1,km
            do j = 1,jm
                do i = 1,im
                    golda(i,j,k) = gold(i,j,k)
                end do
            end do
          end do
        write(31) (((golda(i,j,k),i=1,ipmax),j=1,jpmax),k=1,km)
        end if
        deallocate(golda)


     allocate(holda(ipmax,jpmax,kp))
        call distributefold(holda, fold, ip, jp, kp, ipmax, jpmax, procPerRow)
       if (isMaster()) then
          do k = 1,km
            do j = 1,jm
                do i = 1,im
                    holda(i,j,k) = hold(i,j,k)
                end do
            end do
          end do
        write(31) (((holda(i,j,k),i=1,ipmax),j=1,jpmax),k=1,km)
        close(31)

        end if
        deallocate(holda)

 

        end if



end subroutine ifdata_out



subroutine timestep_out_all_k(n,n0,n1,nmax,km,jm,im,z2,data22,data23,u,w,v,amask1,ut_x1,vt_x1,wt_x1,ut_x2,vt_x2,wt_x2,nspec&
,u_spany2,v_spany2,w_spany2,u_spany3,v_spany3,w_spany3&
,u_x1_19_spany2,v_x1_19_spany2,w_x1_19_spany2,u_x1_19_spany3,v_x1_19_spany3,w_x1_19_spany3)

    use common_sn ! create_new_include_statements() line 102
    real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1) , intent(InOut) :: amask1
    character(len=70), intent(In) :: data22
    character(len=70), intent(In) :: data23
    integer, intent(In) :: im
    integer, intent(In) :: jm
    integer, intent(In) :: km
    integer, intent(In) :: n
    integer, intent(In) :: n0
    integer, intent(In) :: nmax

    integer, intent(In) ::  nspec

    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: u
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: v
    real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(In) :: w
    real(kind=4), dimension(kp+2) , intent(In) :: z2

    real(kind=4), dimension(1,1,nspec+1,kp),intent(out)  :: ut_x1
    real(kind=4), dimension(1,1,nspec+1,kp),intent(out)  :: ut_x2
    real(kind=4), dimension(1,1,nspec+1,kp),intent(out)  :: vt_x1
    real(kind=4), dimension(1,1,nspec+1,kp),intent(out)  :: vt_x2
    real(kind=4), dimension(1,1,nspec+1,kp),intent(out)  :: wt_x1
    real(kind=4), dimension(1,1,nspec+1,kp),intent(out)  :: wt_x2


    real(kind=4), dimension(1,kp,nspec+1),intent(out)  :: u_spany2
    real(kind=4), dimension(1,kp,nspec+1),intent(out)  :: v_spany2
    real(kind=4), dimension(1,kp,nspec+1),intent(out)  :: w_spany2
    real(kind=4), dimension(1,kp,nspec+1),intent(out)  :: u_spany3
    real(kind=4), dimension(1,kp,nspec+1),intent(out)  :: v_spany3
    real(kind=4), dimension(1,kp,nspec+1),intent(out)  :: w_spany3

    real(kind=4), dimension(19,kp,nspec+1),intent(out)  :: u_x1_19_spany2
    real(kind=4), dimension(19,kp,nspec+1),intent(out)  :: v_x1_19_spany2
    real(kind=4), dimension(19,kp,nspec+1),intent(out)  :: w_x1_19_spany2
    real(kind=4), dimension(19,kp,nspec+1),intent(out)  :: u_x1_19_spany3
    real(kind=4), dimension(19,kp,nspec+1),intent(out)  :: v_x1_19_spany3
    real(kind=4), dimension(19,kp,nspec+1),intent(out)  :: w_x1_19_spany3

    integer :: x1,x2,x3,y1,y2,y3,t,nsta


      x1=20
      x2=60
      x3=80
!      y1=1  
      y2=25
      y3=50
!spanwise timestep output,   
     if(n.ge.n1) then
     if(rank.ge.54.and.rank.le.59) then !for using outflow boundary value and this case is procPerRow=6 procPerCol=10

     do k=1,kp
     u_spany2(1,k,n-n1+1) = real(0.5*(u(x3-1,y2,k)+u(x3,y2,k)))
     v_spany2(1,k,n-n1+1) = real(0.5*(v(x3,y2-1,k)+v(x3,y2,k)))
     w_spany2(1,k,n-n1+1) = real(0.5*(w(x3,y2,k-1)+w(x3,y2,k)))
     u_spany3(1,k,n-n1+1) = real(0.5*(u(x3-1,y3,k)+u(x3,y3,k)))
     v_spany3(1,k,n-n1+1) = real(0.5*(v(x3,y3-1,k)+v(x3,y3,k)))
     w_spany3(1,k,n-n1+1) = real(0.5*(w(x3,y3,k-1)+w(x3,y3,k)))
     end do

      if(n.eq.nmax) then

       write(filename, '("uvw_y2_spanwise",i3.3,"_outflow.dat")') rank

!       do t=1,nspec
!        write(*,*) 'u_spany2=',u_spany2(1,10,t)
!        write(*,*) 'v_spany2=',v_spany2(1,10,t)
!        write(*,*) 'w_spany2=',w_spany2(1,10,t)
!       end do

       open(unit=15,file=filename,form='unformatted',status='unknown')
       write(15) ((real(u_spany2(1,k,t)),k=1,kp),t=1,nspec)
       write(15) ((real(v_spany2(1,k,t)),k=1,kp),t=1,nspec)
       write(15) ((real(w_spany2(1,k,t)),k=1,kp),t=1,nspec)

      close(15)


       write(filename, '("uvw_y3_spanwise",i3.3,"_outflow.dat")') rank

!       do t=1,nspec
!        write(*,*) 'u_spany3=',u_spany3(1,10,t)
!        write(*,*) 'v_spany3=',v_spany3(1,10,t)
!        write(*,*) 'w_spany3=',w_spany3(1,10,t)
!       end do

       open(unit=15,file=filename,form='unformatted',status='unknown')
       write(15) ((real(u_spany3(1,k,t)),k=1,kp),t=1,nspec)
       write(15) ((real(v_spany3(1,k,t)),k=1,kp),t=1,nspec)
       write(15) ((real(w_spany3(1,k,t)),k=1,kp),t=1,nspec)

       close(15)



     end if
     end if
     end if



! near-input timestep output
     if(n.ge.n1) then
     if(rank.ge.0.and.rank.le.5) then !for using value near to inflow boundary and this case is procPerRow=6 procPerCol=10

     do k=1,kp
     do i=1,19
     u_x1_19_spany2(i,k,n-n1+1) = real(0.5*(u(i-1,y2,k)+u(i,y2,k)))
     v_x1_19_spany2(i,k,n-n1+1) = real(0.5*(v(i,y2-1,k)+v(i,y2,k)))
     w_x1_19_spany2(i,k,n-n1+1) = real(0.5*(w(i,y2,k-1)+w(i,y2,k)))
     u_x1_19_spany3(i,k,n-n1+1) = real(0.5*(u(i-1,y3,k)+u(i,y3,k)))
     v_x1_19_spany3(i,k,n-n1+1) = real(0.5*(v(i,y3-1,k)+v(i,y3,k)))
     w_x1_19_spany3(i,k,n-n1+1) = real(0.5*(w(i,y3,k-1)+w(i,y3,k)))
     end do
     end do

      if(n.eq.nmax) then

       write(filename, '("uvw_x1_19_y2_spanwise",i3.3,".dat")') rank

!       do t=1,nspec
!        write(*,*) 'u_x1_19_spany2=',u_x1_19_spany2(1,10,t)
!        write(*,*) 'v_x1_19_spany2=',v_x1_19_spany2(1,10,t)
!        write(*,*) 'w_x1_19_spany2=',w_x1_19_spany2(1,10,t)
!       end do

       open(unit=15,file=filename,form='unformatted',status='unknown')
       write(15) (((real(u_x1_19_spany2(i,k,t)),i=1,19),k=1,kp),t=1,nspec)
       write(15) (((real(v_x1_19_spany2(i,k,t)),i=1,19),k=1,kp),t=1,nspec)
       write(15) (((real(w_x1_19_spany2(i,k,t)),i=1,19),k=1,kp),t=1,nspec)

       close(15)


       write(filename, '("uvw_x1_19_y3_spanwise",i3.3,".dat")') rank

!       do t=1,nspec
!        write(*,*) 'u_x1_19_spany3=',u_x1_19_spany3(1,10,t)
!        write(*,*) 'v_x1_19_spany3=',v_x1_19_spany3(1,10,t)
!        write(*,*) 'w_x1_19_spany3=',w_x1_19_spany3(1,10,t)
!       end do

       open(unit=15,file=filename,form='unformatted',status='unknown')
       write(15) (((real(u_x1_19_spany3(i,k,t)),i=1,19),k=1,kp),t=1,nspec)
       write(15) (((real(v_x1_19_spany3(i,k,t)),i=1,19),k=1,kp),t=1,nspec)
       write(15) (((real(w_x1_19_spany3(i,k,t)),i=1,19),k=1,kp),t=1,nspec)

       close(15)

     end if
     end if
     end if



!streamwise timestep output

      if(n.ge.n1) then

      if(mod(rank,6).eq.3) then !for using middle value in domain and this case is procPerRow=6 procPerCol=10
      call MPI_COMM_Rank(communicator, rank, ierror)
      call checkMPIError()
      write(*,*) 'rank=',rank


      nsta = n - n1 + 1

      
      do k=1,kp

! u&v&w are outputed at scalar point

      ut_x1(1,1,nsta,k)=real(0.5*(u(x1-1,y3,k)+u(x1,y3,k)))

      ut_x2(1,1,nsta,k)=real(0.5*(u(x2-1,y3,k)+u(x2,y3,k)))

      vt_x1(1,1,nsta,k)=real(0.5*(v(x1,y3-1,k)+v(x1,y3,k)))

      vt_x2(1,1,nsta,k)=real(0.5*(v(x2,y3-1,k)+v(x2,y3,k)))

      wt_x1(1,1,nsta,k)=real(0.5*(w(x1,y3,k-1)+w(x1,y3,k)))

      wt_x2(1,1,nsta,k)=real(0.5*(w(x2,y3,k-1)+w(x1,y3,k)))


      end do


      if(n.eq.nmax) then

       write(filename, '("uvwt_x1","_",i3.3,"_",i6.6,".dat")') rank,nmax

       open(unit=25,file=filename,form='unformatted',status='unknown')
       write(25) ((real(ut_x1(1,1,t,k)),t=1,nspec),k=1,kp)
       write(25) ((real(vt_x1(1,1,t,k)),t=1,nspec),k=1,kp)
       write(25) ((real(wt_x1(1,1,t,k)),t=1,nspec),k=1,kp)
       
 
       close(25)


       write(filename, '("uvwt_x2","_",i3.3,"_",i6.6,".dat")') rank,nmax

       open(unit=26,file=filename,form='unformatted',status='unknown')
       write(26) ((real(ut_x2(1,1,t,k)),t=1,nspec),k=1,kp)
       write(26) ((real(vt_x2(1,1,t,k)),t=1,nspec),k=1,kp)
       write(26) ((real(wt_x2(1,1,t,k)),t=1,nspec),k=1,kp)


       close(26)

       
    
      end if
      end if
      end if
end subroutine timestep_out_all_k



end module module_anime
