/* Reference list of commands for Problem set 6 */

matrix A = (1,5\1,7\1,3\1,4)
matrix list A

matrix B = (2,8)
matrix list B

matrix AT = A'
matrix list AT

matrix BT = B'
matrix list BT

matrix ATA = AT*A
matrix list ATA, nohalf

matrix ABT = A*BT
matrix list ABT

matrix DetATA = det(ATA)
matrix list DetATA

matrix InvATA = syminv(ATA)
matrix list InvATA, nohalf

matrix TrcATA = trace(ATA)
matrix list TrcATA

matrix Qad = B*InvATA*BT
matrix list Qad, nohalf

matrix Ic = ATA*InvATA
matrix list Ic, nohalf

matrix drop _all
