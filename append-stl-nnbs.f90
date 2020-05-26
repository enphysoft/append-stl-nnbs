! 
! Copyright © 2019-present, by Albert S. Kim
! Author: Albert S. Kim ( http://albertsk.org/ )
! Version: 1.0
! Package-Version: 20190630.0627
! Created: 06/30/2019 
! Keywords: STL, nearest neighbor 
! Description: 
!  This program is, for a selected facet, to generate a list of 
! three nearest facets, sharing an edge, connected by two shared vertices. 
! For example, if a facet 45 and 72 are nearest neighbors, and 
! vertices 1 and 2 of facet 45 and vertices 2 and 3 of facet 72 are
! paired, respectively, the outcome looks like
!  45 72
!   1  2
!   2  3
!   3  0
! where the last line indicates that vertex 3 of facet 45 is not
! paired with any of vertices of facet 72.
!    
! entree = mystl.stl
! output = mystlNNB.dat, facetNNB.dat, checkNNB.dat
!

program append_stl_nnbs
   use mathstl
   implicit none 
   character (128) :: entree_fileSTL
   character (128) :: defaultfileSTL="STL_INPUT.stl"
   character (128) :: output_fileSTL="STL_INPUT_NNBS.stl"
   character (128) :: output_fileNNB="NNBindex.dat"
   character (128) :: review_fileNNB="NNBcheck.dat"
   character (128) :: facets_fileNNB="NNBfacet.dat"
   character (128) :: runtst_NNBlist="NNBlists.dat"
   character (128) :: title_stl
   character (128) :: prefix_nnbed="nnbs_", postfix_nnbed="_nnbs" 
   character (128) :: program_name
   character (128) :: args, arg1, arg1_dir, arg1_file, arg1_root, arg1_extn
   integer :: numtri, numline
   integer :: ipostn_sep, kpostn_sep,  ilenth_arg, ipostn_dot,  ilenth_file
   integer :: ix,jy
   integer :: i_fct, l_fct
   integer :: i_vtx, l_vtx
   integer :: N_fct, N_fct_o10, numPair_i_l
   integer :: nidPair_tmp(3), vetPair_tmp(3), matPair_tmp(3,3)
   integer :: nargc
   real(8) :: vecA(3),vecB(3)
   logical :: sameVec
   integer      , allocatable :: numFacets_i(:)
   type(stl_tri), allocatable :: myFacet(:), loadedFacet(:)
   type(stl_nnb), allocatable :: myNNbor(:), loadedNNbor(:)
   type(stl_tri_nnb) , allocatable :: nnbedFacet(:)
   
   nargc = iargc()
   call getarg(0,program_name)
   
   if ( nargc == 0 )  then
      entree_fileSTL=defaultfileSTL
      ! write(*,*) "Input STL file = ", trim(adjustL(entree_fileSTL))
      
   else if ( nargc == 1) then
      call getarg(1,arg1)
      entree_fileSTL=arg1
      ipostn_sep=scan(arg1,"/", back=.true.) !       ipostn_sep=scan(arg1,"/")
      ilenth_arg= len(arg1)
      arg1_dir  = arg1(1:ipostn_sep)
      arg1_file = arg1(ipostn_sep+1:ilenth_arg)
      ipostn_dot=scan(arg1_file,".", back=.true.)
      ilenth_file= len(arg1_file)
      arg1_root = arg1_file(1:ipostn_dot-1)
      arg1_extn = arg1_file(ipostn_dot:ilenth_file)
      output_fileSTL=trim(adjustl(arg1_root))//trim(adjustl(postfix_nnbed))//trim(adjustL(arg1_extn))
      write(*,*) "Input  STL file: ", trim(adjustL(entree_fileSTL))
      write(*,*) "Output STL file: ", trim(adjustL(output_fileSTL))
   else
      write(*,*) " Correct usage: ", trim(adjustl(program_name))
      write(*,*) "            or: ", trim(adjustl(program_name)), " <stl-file-name>"
      stop
   end if
   
   write(*,"(' entree_fileSTL = ',A)") trim(entree_fileSTL)
   write(*,"(' output_fileSTL = ',A)") trim(output_fileSTL)
   write(*,"(' review_fileNNB = ',A)") trim(review_fileNNB)
   write(*,"(' facets_fileNNB = ',A)") trim(facets_fileNNB)
   write(*,"(' output_fileNNB = ',A)") trim(output_fileNNB)

   numline = cal_num_of_file_lines (entree_fileSTL)
   numtri  = (numline-2)/7
   N_fct    = numtri
   N_fct_o10= N_fct / 10
   
   write(*,*) "number of lines in input file = ", numline
   write(*,*) "number of triangles (facets)  = ", numtri
   allocate(myFacet(numtri), loadedFacet(numtri))
   allocate(myNNbor(numtri), loadedNNbor(numtri))
   allocate(nnbedFacet(numtri), numFacets_i(numtri))
   numFacets_i=0
   
   call read_ascii_stl (entree_fileSTL,numtri,myFacet,title_stl)
   write(*,*) " reading STL file done. "
   
   do i_fct = 1, N_fct
      myNNbor(i_fct).mvtx(1,1:3) = myFacet(i_fct).tvtx.vertexA(1:3)
      myNNbor(i_fct).mvtx(2,1:3) = myFacet(i_fct).tvtx.vertexB(1:3)
      myNNbor(i_fct).mvtx(3,1:3) = myFacet(i_fct).tvtx.vertexC(1:3)
   enddo
   
   open(unit=21,file=review_fileNNB,status="replace")
   write(*,"(F6.2,'% done')")  0.d0
   i_fct_loop: do i_fct = 1, N_fct
      numFacets_i(i_fct) = 0

      l_fct_loop: do l_fct = 1, N_fct
         numPair_i_l = 0
         matPair_tmp = 0
         nidPair_tmp = 0 
         vetPair_tmp = 0             
         do i_vtx = 1, 3
            do l_vtx = 1, 3
               vecA = myNNbor(i_fct).mvtx(i_vtx,1:3)
               vecB = myNNbor(l_fct).mvtx(l_vtx,1:3)
               sameVec = chk_equal_vectors (vecA, vecB)
               if ( sameVec .eq. .true. )  then
                  numPair_i_l = numPair_i_l + 1
                  nidPair_tmp (numPair_i_l) = l_fct
                  matPair_tmp (i_vtx,numPair_i_l) = l_vtx
                  vetPair_tmp (i_vtx) = l_vtx
               endif
            enddo
         enddo
                
         if (numPair_i_l == 2 ) then
            numFacets_i(i_fct) = numFacets_i(i_fct) + 1
            myNNbor(i_fct).nidPair(numFacets_i(i_fct)) = l_fct
            myNNbor(i_fct).matPair(numFacets_i(i_fct),:) = vetPair_tmp(:)
            ! write(21,*) 
            write(21,"(2(I8,2x))") i_fct, l_fct 
            write(21,"(2(I8,2x),6(F12.6,2X))") 1,vetPair_tmp(1), & 
                 myFacet(i_fct).tvtx.vertexA(:), myNNbor(l_fct).mvtx(vetPair_tmp(1),:)
            write(21,"(2(I8,2x),6(F12.6,2X))") 2,vetPair_tmp(2), & 
                 myFacet(i_fct).tvtx.vertexB(:), myNNbor(l_fct).mvtx(vetPair_tmp(2),:)
            write(21,"(2(I8,2x),6(F12.6,2X))") 3,vetPair_tmp(3), &                 
                 myFacet(i_fct).tvtx.vertexC(:), myNNbor(l_fct).mvtx(vetPair_tmp(3),:)
         end if
         
      enddo l_fct_loop

      if ( mod( i_fct,N_fct_o10) == 0 )  then
         write(*,"(F6.2,'% done')") dble(i_fct)/dble(N_fct) *100.d0
      else if (i_fct == N_fct) then
         write(*,"(F6.2,'% done')") dble(i_fct)/dble(N_fct) *100.d0
      end if
         
   enddo i_fct_loop
   close(21)

   open(unit=22,file=output_fileNNB,status="replace")
   do i_fct = 1, N_fct
      write(22,*) "neighbors of ", i_fct, myNNbor(i_fct).nidPair(:)
      do ix = 1, 3
         write(22,*) "pairs of vtx ", ix, myNNbor(i_fct).matPair(:,ix)
      enddo
   enddo
   close(22)
   
   open(unit=23,file=facets_fileNNB,status="replace")
   write(23,"(A)") "solid nearest-neighbor-facets"
   do i_fct = 1, N_fct
      write(23,"(A5,5X,4(2x,I))") "facet", i_fct, myNNbor(i_fct).nidPair(:)
      write(23,"(A4)") "pair" 
      do ix = 1, 3
         write(23,"(10X,4(2x,I))") ix, myNNbor(i_fct).matPair(:,ix)
      enddo
      write(23,"(A7)") "endpair" 
      write(23,"(A8)") "endfacet" 
   enddo
   write(23,"(A)") "endsolid nearest-neighbor-facets"

   call write_ascii_stl_w_nnb_all (output_fileSTL,myFacet,myNNbor,numtri,title_stl)
   call read_ascii_stl_w_nnb_all  (output_fileSTL,loadedFacet,loadedNNbor,numtri,title_stl)
   
   do i_fct = 1, N_fct
      nnbedFacet(i_fct)%fct%tid = i_fct
      nnbedFacet(i_fct)%fct = loadedFacet(i_fct)
      nnbedFacet(i_fct)%nnb = loadedNNbor(i_fct)
   enddo

   open (unit=25,file=runtst_NNBlist,status="replace")
   do i_fct = 1, N_fct
      write(25,"(7(1X,G))") nnbedFacet(i_fct)%fct%tid, & 
           nnbedFacet(i_fct)%nnb%nidPair, nnbedFacet(i_fct)%fct%tvtx%vertexA
   enddo
   close(unit=25) 

   deallocate (myFacet, myNNbor, numFacets_i, loadedNNbor, loadedFacet, nnbedFacet )
   
end program append_stl_nnbs
 
! EOF
