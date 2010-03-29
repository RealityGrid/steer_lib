#
#  The RealityGrid Steering Library
#
#  Copyright (c) 2002-2010, University of Manchester, United Kingdom.
#  All rights reserved.
#
#  This software is produced by Research Computing Services, University
#  of Manchester as part of the RealityGrid project and associated
#  follow on projects, funded by the EPSRC under grants GR/R67699/01,
#  GR/R67699/02, GR/T27488/01, EP/C536452/1, EP/D500028/1,
#  EP/F00561X/1.
#
#  LICENCE TERMS
#
#  Redistribution and use in source and binary forms, with or without
#  modification, are permitted provided that the following conditions
#  are met:
#
#    * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#
#    * Redistributions in binary form must reproduce the above
#      copyright notice, this list of conditions and the following
#      disclaimer in the documentation and/or other materials provided
#      with the distribution.
#
#    * Neither the name of The University of Manchester nor the names
#      of its contributors may be used to endorse or promote products
#      derived from this software without specific prior written
#      permission.
#
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
#  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
#  COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
#  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
#  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
#  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
#  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
#  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
#  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
#  POSSIBILITY OF SUCH DAMAGE.
#
#  Author: Robert Haines

# work out which sockets header we have
CHECK_INCLUDE_FILES("sys/socket.h" REG_HAS_SYS_SOCKET_H)
CHECK_INCLUDE_FILES("Winsock2.h" REG_HAS_WINSOCK2_H)

if(REG_HAS_SYS_SOCKET_H)
  set(REG_TEST_SOCKETS_H "sys/socket.h")
endif(REG_HAS_SYS_SOCKET_H)
if(REG_HAS_WINSOCK2_H)
  set(REG_TEST_SOCKETS_H "Winsock2.h")
  set(REG_HAS_CLOSESOCKET 1)
endif(REG_HAS_WINSOCK2_H)

# test sockets features
if(REG_TEST_SOCKETS_H)
  CHECK_SYMBOL_EXISTS(MSG_NOSIGNAL ${REG_TEST_SOCKETS_H} REG_HAS_MSG_NOSIGNAL)
  CHECK_SYMBOL_EXISTS(MSG_DONTWAIT ${REG_TEST_SOCKETS_H} REG_HAS_MSG_DONTWAIT)
  CHECK_SYMBOL_EXISTS(MSG_WAITALL  ${REG_TEST_SOCKETS_H} REG_HAS_MSG_WAITALL)
endif(REG_TEST_SOCKETS_H)
