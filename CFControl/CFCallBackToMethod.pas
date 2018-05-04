unit CFCallBackToMethod;

{*******************************************
 * brief: 回调函数转对象方法的实现
 * autor: linzhenqun
 * date:  2006-12-18
 * email: linzhengqun@163.com
********************************************}
{
说明：本单元的实现方法是一种比较安全的方式，其中不破坏任何寄存器的值，并且
      指令的大小只有16字节。
使用：下面是推荐的使用方法
      1. 在类中保存一个指针成员 P: Pointer
      2. 在类的构造函数中创建指令块：
         var
           M: TMethod;
         begin
           M.Code := @MyMethod;
           M.Data := Self;
           P := MakeInstruction(M);
         end;
      3. 调用需要回调函数的API时，直接传进P即可，如：
         HHK := SetWindowsHookEx(WH_KEYBOARD, P, HInstance, 0);
      4. 在类的析构函数中释放指令块
         FreeInstruction(P);
注意：作为回调函数的对象方法必须是StdCall调用规则
}

interface

(* 创建回调函数转对象方法的指令块 *)
function MakeInstruction(Method: TMethod): Pointer;
(* 消毁指令块 *)
procedure FreeInstruction(P: Pointer);

implementation

uses SysUtils;

type
  {
    指令块中的内容相当于下面的汇编代码：
    ----------------------------------
    push  [ESP]
    mov   [ESP+4], ObjectAddr
    jmp   MethodAddr
    ----------------------------------
  }
  PInstruction = ^TInstruction;
  TInstruction = packed record
    Code1: array [0..6] of byte;
    Self: Pointer;
    Code2: byte;
    Method: Pointer;
  end;

function MakeInstruction(Method: TMethod): Pointer;
const
  Code: array[0..15] of byte =
   ($FF,$34,$24,$C7,$44,$24,$04,$00,$00,$00,$00,$E9,$00,$00,$00,$00);
var
  P: PInstruction;
begin
  New(P);
  Move(Code, P^, SizeOf(Code));
  P^.Self := Method.Data;
  P^.Method := Pointer(Longint(Method.Code)-(Longint(P)+SizeOf(Code)));
  Result := P;
end;

procedure FreeInstruction(P: Pointer);
begin
  Dispose(P);
end;

end.
