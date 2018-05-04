unit DiocpError;

interface

const
  WSABASEERR              = 10000;

{ Windows Sockets definitions of regular Microsoft C error constants }

  {$EXTERNALSYM WSAEINTR}
  WSAEINTR                = (WSABASEERR+4);   // 函数调用中断。该错误表明由于对WSACancelBlockingCall的调用，造成了一次调用被强行中断。
  {$EXTERNALSYM WSAEBADF}
  WSAEBADF                = (WSABASEERR+9);   // 文件句柄错误。该错误表明提供的文件句柄无效。
  {$EXTERNALSYM WSAEACCES}
  WSAEACCES               = (WSABASEERR+13);  // 权限被拒。尝试对套接字进行操作，但被禁止。若试图在sendto或WSASendTo中使用一个广播地址，但是尚未用setsockopt和SO_BROADCAST这两个选项设置广播权限，便会产生这类错误。
  {$EXTERNALSYM WSAEFAULT}
  WSAEFAULT               = (WSABASEERR+14);  // 地址无效。传给Winsock函数的指针地址无效。若指定的缓冲区太小，也会产生这个错误。
  {$EXTERNALSYM WSAEINVAL}
  WSAEINVAL               = (WSABASEERR+22);  // 参数无效。指定了一个无效参数。例如，假如为WSAIoctl调用指定了一个无效控制代码，便会产生这个错误。另外，它也可能表明套接字当前的状态有错，例如在一个目前没有监听的套接字上调用accept或WSAAccept。
  {$EXTERNALSYM WSAEMFILE}
  WSAEMFILE               = (WSABASEERR+24);  // 打开文件过多。提示打开的套接字太多了。通常，Microsoft提供者只受到系统内可用资源数量的限制。

{ Windows Sockets definitions of regular Berkeley error constants }

  {$EXTERNALSYM WSAEWOULDBLOCK}
  WSAEWOULDBLOCK          = (WSABASEERR+35);  // 套接字标记为未分块，而操作将分块
  {$EXTERNALSYM WSAEINPROGRESS}
  WSAEINPROGRESS          = (WSABASEERR+36);  // 资源暂时不可用。对非锁定套接字来说，如果请求操作不能立即执行的话，通常会返回这个错误。比如说，在一个非暂停套接字上调用conn ect，就会返回这个错误。因为连接请求不能立即执行。
  {$EXTERNALSYM WSAEALREADY}
  WSAEALREADY             = (WSABASEERR+37);  // 操作已完成。一般来说，在非锁定套接字上尝试已处于进程中的操作时，会产生这个错误。比如，在一个已处于连接进程的非锁定套接字上，再一次调用connect或WSAConnect。另外，服务提供者处于执行回调函数
  {$EXTERNALSYM WSAENOTSOCK}
  WSAENOTSOCK             = (WSABASEERR+38);  // 无效套接字上的套接字操作。任何一个把SOCKET句柄当作参数的Winsock函数都会返回这个错误。它表明提供的套接字句柄无效。
  {$EXTERNALSYM WSAEDESTADDRREQ}
  WSAEDESTADDRREQ         = (WSABASEERR+39);  // 需要目标地址。这个错误表明没有提供具体地址。比方说，假如在调用sendto时，将目标地址设为INADDR_ANY（任意地址），便会返回这个错误。
  {$EXTERNALSYM WSAEMSGSIZE}
  WSAEMSGSIZE             = (WSABASEERR+40);  // 消息过长。这个错误的含义很多。如果在一个数据报套接字上发送一条消息，这条消息对内部缓冲区而言太大的话，就会产生这个错误。再比如，由于网络本身的限制，使一条消息过长，也会产生这个错误。最后，如果收到数据报之后，缓冲区太小，不能接收消息时，也会产生这个错误。
  {$EXTERNALSYM WSAEPROTOTYPE}
  WSAEPROTOTYPE           = (WSABASEERR+41);  // 套接字协议类型有误。在socket或WSASocket 调用中指定的协议不支持指定的套接字类型。比如，要求建立SOCK_STREAM类型的一个IP套接字，同时指定协议为IPPROTO_UDP，便会产生这样的错误。
  {$EXTERNALSYM WSAENOPROTOOPT}
  WSAENOPROTOOPT          = (WSABASEERR+42);  // 协议选项错误。表明在getsockopt或setsockopt 调用中，指定的套接字选项或级别不明、未获支持或者无效。
  {$EXTERNALSYM WSAEPROTONOSUPPORT}
  WSAEPROTONOSUPPORT      = (WSABASEERR+43);  // 不支持的协议。系统中没有安装请求的协议或没有相应的实施方案。比如，如果系统中没有安装TCP/IP，而试着建立TCP或UDP套接字时，就会产生这个错误。
  {$EXTERNALSYM WSAESOCKTNOSUPPORT}
  WSAESOCKTNOSUPPORT      = (WSABASEERR+44);  // 不支持的套接字类型。对指定的地址家族来说，没有相应的具体套接字类型支持。比如，在向一个不支持原始套接字的协议请求建立一个SOCK_RAW套接字类型时，就会产生这个错误。
  {$EXTERNALSYM WSAEOPNOTSUPP}
  WSAEOPNOTSUPP           = (WSABASEERR+45);  // 不支持的操作。表明针对指定的对象，试图采取的操作未获支持。通常，如果试着在一个不支持调用Winsock函数的套接字上调用了Winsock时，就会产生这个错误。
  {$EXTERNALSYM WSAEPFNOSUPPORT}
  WSAEPFNOSUPPORT         = (WSABASEERR+46);  // 不支持的协议家族。请求的协议家族不存在，或系统内尚未安装。多数情况下，这个错误可与WSAEAFNOSUPPORT互换（两者等价）；后者出现得更为频繁。
  {$EXTERNALSYM WSAEAFNOSUPPORT}
  WSAEAFNOSUPPORT         = (WSABASEERR+47);  // 地址家族不支持请求的操作。对套接字类型不支持的操作来说，在试着执行它时，就会出现这个错误。比如，在类型为SOCK_STREAM的一个套接字上调用sendto或WSASendTo函数时，就会产生这个错误。另外，在调用socket或WSASocket函数的时候，若同时请求了一个无效的地址家族、套接字类型及协议组合，也会产生这个错误。
  {$EXTERNALSYM WSAEADDRINUSE}
  WSAEADDRINUSE           = (WSABASEERR+48);  // 地址正在使用。正常情况下，每个套接字只允许使用一个套接字地址这个错误一般和bind、connect和WSAConnect这三个函数有关。可在setsockopt函数中设置套接字选项SO_REUSEA D D R ，允许多个套接字访问同一个本地I P 地址及端口号
  {$EXTERNALSYM WSAEADDRNOTAVAIL}
  WSAEADDRNOTAVAIL        = (WSABASEERR+49);  // 不能分配请求的地址。API调用中指定的地址对那个函数来说无效时，就会产生这样的错误。例如，若在bind调用中指定一个IP地址，但却没有对应的本地IP接口，便会产生这样的错误。另外，通过connect、WSAConnect、sendto、WSASendTo和WSAJoinLeaf这四个函数为准备连接的远程计算机指定端口0时，也会产生这样的错误。
  {$EXTERNALSYM WSAENETDOWN}
  WSAENETDOWN             = (WSABASEERR+50);  // 网络断开。试图采取一项操作时，却发现网络连接中断。这可能是由于网络堆栈的错误，网络接口的故障，或者本地网络的问题造成的。
  {$EXTERNALSYM WSAENETUNREACH}
  WSAENETUNREACH          = (WSABASEERR+51);  // 网络不可抵达。试图采取一项操作时，却发现目标网络不可抵达（不可访问）。这意味着本地主机不知道如何抵达一个远程主机。换言之，目前没有已知的路由可抵达那个目标主机。
  {$EXTERNALSYM WSAENETRESET}
  WSAENETRESET            = (WSABASEERR+52);  // 网络重设时断开了连接。由于“保持活动”操作检测到一个错误，造成网络连接的中断。若在一个已经无效的连接之上，通过setsockopt函数设置SO_KEEPALIVE选项，也会出现这样的错误。
  {$EXTERNALSYM WSAECONNABORTED}
  WSAECONNABORTED         = (WSABASEERR+53);  // 软件造成连接取消。由于软件错误，造成一个已经建立的连接被取消。典型情况下，这意味着连接是由于协议或超时错误而被取消的。
  {$EXTERNALSYM WSAECONNRESET}
  WSAECONNRESET           = (WSABASEERR+54);  // 连接被对方重设。一个已经建立的连接被远程主机强行关闭。若远程主机上的进程异常中止运行（由于内存冲突或硬件故障），或者针对套接字执行了一次强行关闭，便会产生这样的错误。针对强行关闭的情况，可用SO_LINGER套接字选项和setsockopt来配置一个套接字
  {$EXTERNALSYM WSAENOBUFS}
  WSAENOBUFS              = (WSABASEERR+55);  // 没有缓冲区空间。由于系统缺少足够的缓冲区空间，请求的操作不能执行。
  {$EXTERNALSYM WSAEISCONN}
  WSAEISCONN              = (WSABASEERR+56);  // 套接字已经连接。表明在一个已建立连接的套接字上，试图再建立一个连接。要注意的是，数据报和数据流套接字均有可能出现这样的错误。使用数据报套接字时，假如事先已通过connect或WSAConnect调用，为数据报通信关联了一个端点的地址，那么以后试图再次调用sendto或WSASendTo，便会产生这样的错误。
  {$EXTERNALSYM WSAENOTCONN}
  WSAENOTCONN             = (WSABASEERR+57);  // 套接字尚未连接。若在一个尚未建立连接的“面向连接”套接字上发出数据收发请求，便会产生这样的错误。
  {$EXTERNALSYM WSAESHUTDOWN}
  WSAESHUTDOWN            = (WSABASEERR+58);  // 套接字关闭后不能发送。表明已通过对shutdown的一次调用，部分关闭了套接字，但后又请求进行数据的收发操作。要注意的是，这种错误只会在已经关闭的那个数据流动方向上才会发生。举个例子来说，完成数据发送后，若调用shutdown，那么以后任何数据发送调用都会产生这样的错误。
  {$EXTERNALSYM WSAETOOMANYREFS}
  WSAETOOMANYREFS         = (WSABASEERR+59);
  {$EXTERNALSYM WSAETIMEDOUT}
  WSAETIMEDOUT            = (WSABASEERR+60);  // 连接超时。若发出了一个连接请求，但经过规定的时间，远程计算机仍未作出正确的响应（或根本没有任何响应），便会发生这样的错误。要想收到这样的错误，通常需要先在套接字上设置好SO_SNDTIMEO和SO_RCVTIMEO选项，然后调用connect及WSAConnect函数。
  {$EXTERNALSYM WSAECONNREFUSED}
  WSAECONNREFUSED         = (WSABASEERR+61);  // 连接被拒。由于被目标机器拒绝，连接无法建立。这通常是由于在远程机器上，没有任何应用程序可在那个地址之上，为连接提供服务。
  {$EXTERNALSYM WSAELOOP}
  WSAELOOP                = (WSABASEERR+62);
  {$EXTERNALSYM WSAENAMETOOLONG}
  WSAENAMETOOLONG         = (WSABASEERR+63);
  {$EXTERNALSYM WSAEHOSTDOWN}
  WSAEHOSTDOWN            = (WSABASEERR+64); // 主机关闭。这个错误指出由于目标主机关闭，造成操作失败。然而，应用程序此时更有可能收到的是一条WSAETIMEDOUT（连接超时）错误，因为对方关机的情况通常是在试图建立一个连接的时候发生的。
  {$EXTERNALSYM WSAEHOSTUNREACH}
  WSAEHOSTUNREACH         = (WSABASEERR+65); // 没有到主机的路由。应用程序试图访问一个不可抵达的主机。该错误类似于WSAENETUNREACH。
  {$EXTERNALSYM WSAENOTEMPTY}
  WSAENOTEMPTY            = (WSABASEERR+66);
  {$EXTERNALSYM WSAEPROCLIM}
  WSAEPROCLIM             = (WSABASEERR+67);
  {$EXTERNALSYM WSAEUSERS}
  WSAEUSERS               = (WSABASEERR+68);
  {$EXTERNALSYM WSAEDQUOT}
  WSAEDQUOT               = (WSABASEERR+69);
  {$EXTERNALSYM WSAESTALE}
  WSAESTALE               = (WSABASEERR+70);
  {$EXTERNALSYM WSAEREMOTE}
  WSAEREMOTE              = (WSABASEERR+71);

  {$EXTERNALSYM WSAEDISCON}
  WSAEDISCON              = (WSABASEERR+101);

{ Extended Windows Sockets error constant definitions }

  {$EXTERNALSYM WSASYSNOTREADY}
  WSASYSNOTREADY          = (WSABASEERR+91);  // 网络子系统不可用。调用WSAStartup时，若提供者不能正常工作（由于提供服务的基层系统不可用），便会返回这种错误。
  {$EXTERNALSYM WSAVERNOTSUPPORTED}
  WSAVERNOTSUPPORTED      = (WSABASEERR+92);  // Winsock.dll版本有误。表明不支持请求的Winsock提供者版本。
  {$EXTERNALSYM WSANOTINITIALISED}
  WSANOTINITIALISED       = (WSABASEERR+93);  // Winsock尚未初始化。尚未成功完成对WSAStartup的一次调用。

{ Error return codes from gethostbyname() and gethostbyaddr()
  (when using the resolver). Note that these errors are
  retrieved via WSAGetLastError() and must therefore follow
  the rules for avoiding clashes with error numbers from
  specific implementations or language run-time systems.
  For this reason the codes are based at WSABASEERR+1001.
  Note also that [WSA]NO_ADDRESS is defined only for
  compatibility purposes. }

{ Authoritative Answer: Host not found }

  {$EXTERNALSYM WSAHOST_NOT_FOUND}
  WSAHOST_NOT_FOUND       = (WSABASEERR+1001);  // 找不到主机
  {$EXTERNALSYM HOST_NOT_FOUND}
  HOST_NOT_FOUND          = WSAHOST_NOT_FOUND;

{ Non-Authoritative: Host not found, or SERVERFAIL }

  {$EXTERNALSYM WSATRY_AGAIN}
  WSATRY_AGAIN            = (WSABASEERR+1002);  // 找不到主机。请求从名称服务器中检索主机名的 IP 地址失败
  {$EXTERNALSYM TRY_AGAIN}
  TRY_AGAIN               = WSATRY_AGAIN;

{ Non recoverable errors, FORMERR, REFUSED, NOTIMP }

  {$EXTERNALSYM WSANO_RECOVERY}
  WSANO_RECOVERY          = (WSABASEERR+1003);
  {$EXTERNALSYM NO_RECOVERY}
  NO_RECOVERY             = WSANO_RECOVERY;

{ Valid name, no data record of requested type }

  {$EXTERNALSYM WSANO_DATA}
  WSANO_DATA              = (WSABASEERR+1004);  // 名称无效，没有请求的类型的数据记录。名称服务器或 hosts 文件不识别主机名，或者在 services 文件中未指定服务名
  {$EXTERNALSYM NO_DATA}
  NO_DATA                 = WSANO_DATA;

{ no address, look for MX record }

  {$EXTERNALSYM WSANO_ADDRESS}
  WSANO_ADDRESS           = WSANO_DATA;
  {$EXTERNALSYM NO_ADDRESS}
  NO_ADDRESS              = WSANO_ADDRESS;

{ Windows Sockets errors redefined as regular Berkeley error constants.
  These are commented out in Windows NT to avoid conflicts with errno.h.
  Use the WSA constants instead. }

  {$EXTERNALSYM EWOULDBLOCK}
  EWOULDBLOCK        =  WSAEWOULDBLOCK;
  {$EXTERNALSYM EINPROGRESS}
  EINPROGRESS        =  WSAEINPROGRESS;
  {$EXTERNALSYM EALREADY}
  EALREADY           =  WSAEALREADY;
  {$EXTERNALSYM ENOTSOCK}
  ENOTSOCK           =  WSAENOTSOCK;
  {$EXTERNALSYM EDESTADDRREQ}
  EDESTADDRREQ       =  WSAEDESTADDRREQ;
  {$EXTERNALSYM EMSGSIZE}
  EMSGSIZE           =  WSAEMSGSIZE;
  {$EXTERNALSYM EPROTOTYPE}
  EPROTOTYPE         =  WSAEPROTOTYPE;
  {$EXTERNALSYM ENOPROTOOPT}
  ENOPROTOOPT        =  WSAENOPROTOOPT;
  {$EXTERNALSYM EPROTONOSUPPORT}
  EPROTONOSUPPORT    =  WSAEPROTONOSUPPORT;
  {$EXTERNALSYM ESOCKTNOSUPPORT}
  ESOCKTNOSUPPORT    =  WSAESOCKTNOSUPPORT;
  {$EXTERNALSYM EOPNOTSUPP}
  EOPNOTSUPP         =  WSAEOPNOTSUPP;
  {$EXTERNALSYM EPFNOSUPPORT}
  EPFNOSUPPORT       =  WSAEPFNOSUPPORT;
  {$EXTERNALSYM EAFNOSUPPORT}
  EAFNOSUPPORT       =  WSAEAFNOSUPPORT;
  {$EXTERNALSYM EADDRINUSE}
  EADDRINUSE         =  WSAEADDRINUSE;
  {$EXTERNALSYM EADDRNOTAVAIL}
  EADDRNOTAVAIL      =  WSAEADDRNOTAVAIL;
  {$EXTERNALSYM ENETDOWN}
  ENETDOWN           =  WSAENETDOWN;
  {$EXTERNALSYM ENETUNREACH}
  ENETUNREACH        =  WSAENETUNREACH;
  {$EXTERNALSYM ENETRESET}
  ENETRESET          =  WSAENETRESET;
  {$EXTERNALSYM ECONNABORTED}
  ECONNABORTED       =  WSAECONNABORTED;
  {$EXTERNALSYM ECONNRESET}
  ECONNRESET         =  WSAECONNRESET;
  {$EXTERNALSYM ENOBUFS}
  ENOBUFS            =  WSAENOBUFS;
  {$EXTERNALSYM EISCONN}
  EISCONN            =  WSAEISCONN;
  {$EXTERNALSYM ENOTCONN}
  ENOTCONN           =  WSAENOTCONN;
  {$EXTERNALSYM ESHUTDOWN}
  ESHUTDOWN          =  WSAESHUTDOWN;
  {$EXTERNALSYM ETOOMANYREFS}
  ETOOMANYREFS       =  WSAETOOMANYREFS;
  {$EXTERNALSYM ETIMEDOUT}
  ETIMEDOUT          =  WSAETIMEDOUT;
  {$EXTERNALSYM ECONNREFUSED}
  ECONNREFUSED       =  WSAECONNREFUSED;
  {$EXTERNALSYM ELOOP}
  ELOOP              =  WSAELOOP;
  {$EXTERNALSYM ENAMETOOLONG}
  ENAMETOOLONG       =  WSAENAMETOOLONG;
  {$EXTERNALSYM EHOSTDOWN}
  EHOSTDOWN          =  WSAEHOSTDOWN;
  {$EXTERNALSYM EHOSTUNREACH}
  EHOSTUNREACH       =  WSAEHOSTUNREACH;
  {$EXTERNALSYM ENOTEMPTY}
  ENOTEMPTY          =  WSAENOTEMPTY;
  {$EXTERNALSYM EPROCLIM}
  EPROCLIM           =  WSAEPROCLIM;
  {$EXTERNALSYM EUSERS}
  EUSERS             =  WSAEUSERS;
  {$EXTERNALSYM EDQUOT}
  EDQUOT             =  WSAEDQUOT;
  {$EXTERNALSYM ESTALE}
  ESTALE             =  WSAESTALE;
  {$EXTERNALSYM EREMOTE}
  EREMOTE            =  WSAEREMOTE;

  function GetDiocpErrorMessage(const AErrCode: Integer): string;

implementation

function GetDiocpErrorMessage(const AErrCode: Integer): string;
begin
  case AErrCode of
    WSAETIMEDOUT: Result := '服务端没有响应，连接超时！';
    WSAECONNREFUSED: Result := '无法连接到服务端，请开启或检查网络！';
    WSAECONNRESET: Result := '服务端关闭，通讯中断！';
  else
    Result := '';
  end;
end;

end.
