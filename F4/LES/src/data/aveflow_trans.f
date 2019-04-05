c
      include 'common.sn'
c
       im=ip
       jm=jp
       km=kp
       write(*,*) 'im,jm,km=',im,jm,km
!------Urban model----------
      do k=1,km
      do j=1,jm
      do i=1,im
        amask1(i,j,k)=1.  !air
      end do
      end do
      end do

c--dz set
      do k=0,1
        z2(k)= 1.
        dzn(k)= 1.
      end do
      do k=2,15
        dzn(k)=dzn(k-1)*1.1
      end do
      do k=16,44
        dzn(k)=4.
      end do
      do k=45,58
        dzn(k)=dzn(k-1)*1.1
      end do
      do k=59,km+1
        dzn(k)=16.
      enddo
      do k=2,km+1
        z2(k)=z2(k-1)+dzn(k)  !Height
      end do


!!choose GIS


      open(70,file='./GIS/DEM_LES_GIS.txt',form='formatted'
     &,status='unknown')

      do j=1,750
        do i=1,3000
          read(70,*) dem(i,j)
        end do
      end do
      close(70)

      open(71,file='./GIS/DSM_LES_GIS.txt',form='formatted'
     &,status='unknown')

      do j=1,750
        do i=1,3000
          read(71,*) dsm(i,j)
        end do
      end do
      close(71)


!this domain is using 11km(the north to south direction) × 1km(the west to east direction) in Kyoto city
      do j=1,250
        do i=1,2750
!if we use this setting for zbm, it can include our observation point, our observation point is (i,j)=(2788,152) in our observation point  
          zbm(i+125,j+25)=dsm(i+250,j+138)-dem(i+250,j+138)
        end do
      end do



c-----------------------------------------------------------------------
!      write(*,*) 'z1=',z2(1)+0.5*dzn(1)

      do j=1,jm
      do i=1,im
      do k=1,km
      if(zbm(i,j).gt.z2(k)+0.5*dzn(k)) then
       amask1(i,j,k)=0.0  !Inside buildings and terrain     
      end if
      end do
      end do 
      end do


c-----------------------------------------------------------------------


        open(unit=10,file='data10',form='unformatted',status='old')


        read(10) (((aveu(i,j,k),i=1,im),j=1,jm),k=1,km)
        read(10) (((avew(i,j,k),i=1,im),j=1,jm),k=1,km)
        read(10) (((avev(i,j,k),i=1,im),j=1,jm),k=1,km)
        close(unit=10)

 
        open(unit=11,file='data11',form='unformatted',status='old')
        read(11) (((aveuu(i,j,k),i=1,im),j=1,jm),k=1,km)
        read(11) (((aveww(i,j,k),i=1,im),j=1,jm),k=1,km)
        read(11) (((avevv(i,j,k),i=1,im),j=1,jm),k=1,km)
        read(11) (((uwfx(i,j,k),i=1,im),j=1,jm),k=1,km)
        close(unit=11)


        do k=1,km
        do i=1,im
          avesssu(i,k)=0.0
          avesssv(i,k)=0.0
          avesssw(i,k)=0.0
          avesssuu(i,k)=0.0
          avesssvv(i,k)=0.0
          avesssww(i,k)=0.0
          uwfxsss(i,k)=0.0
        end do
        end do 

        do k=1,km
       avessu(k)=0.0
       avessv(k)=0.0
       avessw(k)=0.0
       avessuu(k)=0.0
       avessvv(k)=0.0
       avessww(k)=0.0       
       uwfxss(k)=0.0
        end do
c
c
!except buildings |  y ave
      do k=1,km
      do j=1,jm
      do i=1,im
      if(amask1(i,j,k).eq.1.) then
      js(i,k)=js(i,k)+1
        avesssu(i,k)=avesssu(i,k)+aveu(i,j,k)
        avesssv(i,k)=avesssv(i,k)+avev(i,j,k)
        avesssw(i,k)=avesssw(i,k)+avew(i,j,k)
        avesssuu(i,k)=avesssuu(i,k)+aveuu(i,j,k)
        avesssvv(i,k)=avesssvv(i,k)+avevv(i,j,k)
        avesssww(i,k)=avesssww(i,k)+aveww(i,j,k)
        uwfxsss(i,k)=uwfxsss(i,k)+uwfx(i,j,k)
      end if
      end do
      end do
      end do


      do k=1,km
       write(*,*) 'js(i,k)=',js(100,k)
       do i=1,im
        avesssu(i,k)=avesssu(i,k)/float(js(i,k))
        avesssv(i,k)=avesssv(i,k)/float(js(i,k))
        avesssw(i,k)=avesssw(i,k)/float(js(i,k))
        avesssuu(i,k)=avesssuu(i,k)/float(js(i,k))
        avesssvv(i,k)=avesssvv(i,k)/float(js(i,k))
        avesssww(i,k)=avesssww(i,k)/float(js(i,k))
        uwfxsss(i,k)=uwfxsss(i,k)/float(js(i,k))
       end do
      end do


!except buildings | x&y ave
      do k=1,km
      do j=1,jm
      do i=1,im
      if(amask1(i,j,k).eq.1.) then        
      ijs(k)=ijs(k)+1
        avessu(k)=avessu(k)+aveu(i,j,k)
        avessv(k)=avessv(k)+avev(i,j,k)
        avessw(k)=avessw(k)+avew(i,j,k)
        avessuu(k)=avessuu(k)+aveuu(i,j,k)
        avessvv(k)=avessvv(k)+avevv(i,j,k)
        avessww(k)=avessww(k)+aveww(i,j,k)
        uwfxss(k)=uwfxss(k)+uwfx(i,j,k)
      end if
      end do
      end do
      end do


      do k=1,km
        write(*,*) 'ijs(k)=',ijs(k)
        avessu(k)=avessu(k)/float(ijs(k))
        avessv(k)=avessv(k)/float(ijs(k))
        avessw(k)=avessw(k)/float(ijs(k))
        avessuu(k)=avessuu(k)/float(ijs(k))
        avessvv(k)=avessvv(k)/float(ijs(k))
        avessww(k)=avessww(k)/float(ijs(k))
        uwfxss(k)=uwfxss(k)/float(ijs(k))
      end do


!boundary
            do k = 1,km
                do j = 1,jm
                    aveu(0,j,k) = aveu(1,j,k)
                    aveuu(0,j,k) = aveuu(1,j,k)
                    avesssu(0,k) =  avesssu(1,k)
                    avesssuu(0,k) =  avesssuu(1,k)
                end do
            end do


            do j = 1,jm
                do i = 1,im
                    avew(i,j,0) = 0.0
                    aveww(i,j,0) = 0.0
                    avesssw(i,0) =  0.0
                    avesssww(i,0) =  0.0
                end do
            end do


            do k = 1,km
                do i = 1,im
                    avev(i,0,k) = avev(i,jm,k)
                    avevv(i,0,k) = avevv(i,jm,k)
                end do
            end do


      open(unit=10,file='3_data10',form='unformatted',status='replace',
     &access='direct',recl=4*jm*im)
       irec = 1

       do  k=1,km
       write(10,rec=irec) ((real(0.5*(aveu(i-1,j,k)+aveu(i,j,k)))
     &,i=1,im) ,j=1,jm)
       irec = irec + 1
         end do

       do k=1,km
       write(10,rec=irec) ((real(0.5*(avew(i,j,k-1)+avew(i,j,k)))
     &,i=1,im) ,j=1,jm)
       irec = irec + 1
       end do

       do k=1,km
       write(10,rec=irec) ((real(0.5*(avev(i,j-1,k)+avev(i,j,k)))
     &,i=1,im) ,j=1,jm)
       irec = irec + 1
       end do

!       do k=1,km
!       write(10,rec=irec) ((real(avecrlx(i,j,k)),i=1,im),j=1,jm)
!       irec= irec + 1
!       end do

!       do k=1,km
!       write(10,rec=irec) ((real(avecrly(i,j,k)),i=1,im),j=1,jm)
!       irec= irec + 1
!       end do

!       do k=1,km
!       write(10,rec=irec) ((real(avecrlz(i,j,k)),i=1,im),j=1,jm)
!       irec= irec + 1
!       end do


       close(unit=10)

      open(unit=11,file='3_data11',form='unformatted',status='replace',
     &access='direct',recl=4*im*jm)

      irec=1
       do  k=1,km
       write(11,rec=irec) ((real(0.5*(aveuu(i-1,j,k)+aveuu(i,j,k)))
     &,i=1,im) ,j=1,jm)
       irec = irec + 1
         end do

       do k=1,km
       write(11,rec=irec) ((real(0.5*(aveww(i,j,k-1)+aveww(i,j,k)))
     &,i=1,im) ,j=1,jm)
       irec = irec + 1
       end do

       do k=1,km
       write(11,rec=irec) ((real(0.5*(avevv(i,j-1,k)+avevv(i,j,k)))
     &,i=1,im) ,j=1,jm)
       irec = irec + 1
       end do

       do k=1,km
       write(11,rec=irec) ((real(uwfx(i,j,k)),i=1,im),j=1,jm)
       irec = irec + 1
      end do

       close(unit=11)

c

      open(unit=13,file='3_data13',form='unformatted',status='replace',
     &access='direct',recl=4)
     
              irec=1
       do k=1,km
       write(13,rec=irec) (real(avessu(k)))
       irec = irec + 1
       end do
       do k=1,km
       write(13,rec=irec) (real(0.5*(avessw(k-1)+avessw(k))))
       irec = irec + 1
       end do
       do k=1,km
        write(13,rec=irec) (real(avessv(k)))
       irec = irec + 1
       end do
       do k=1,km
       write(13,rec=irec) (real(avessuu(k)))
       irec = irec + 1
       end do
       do k=1,km
        write(13,rec=irec) (real(0.5*(avessww(k-1)+avessww(k))))
       irec = irec + 1
       end do
       do k=1,km
        write(13,rec=irec) (real(avessvv(k)))
       irec = irec + 1
       end do
       do k=1,km
        write(13,rec=irec) (real(uwfxss(k)))
       irec = irec + 1
       end do
       close(unit=13)


      open(unit=14,file='3_data14',form='unformatted',status='replace',
     &access='direct',recl=4*im)

      irec=1
       do  k=1,km
       write(14,rec=irec) (real(0.5*(avesssu(i-1,k)+avesssu(i,k)))
     &,i=1,im)
       irec = irec + 1
         end do

       do k=1,km
       write(14,rec=irec) (real(0.5*(avesssw(i,k-1)+avesssw(i,k)))
     &,i=1,im)
       irec = irec + 1
       end do

       do k=1,km
       write(14,rec=irec) (real(avesssv(i,k))
     &,i=1,im)
       irec = irec + 1
       end do


       do  k=1,km
       write(14,rec=irec) (real(0.5*(avesssuu(i-1,k)+avesssuu(i,k)))
     &,i=1,im)
       irec = irec + 1
         end do

       do k=1,km
       write(14,rec=irec) (real(0.5*(avesssww(i,k-1)+avesssww(i,k)))
     &,i=1,im)
       irec = irec + 1
       end do

       do k=1,km
       write(14,rec=irec) (real(avesssvv(i,k))
     &,i=1,im)
       irec = irec + 1
       end do

       do k=1,km
       write(14,rec=irec) (real(uwfxsss(i,k)),i=1,im)
       irec = irec + 1
      end do

       close(unit=14)

      end                                           
