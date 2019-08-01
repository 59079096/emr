{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit HCEmrGridView;

interface

uses
  Windows, Classes, Controls, Vcl.Graphics, HCGridView, HCStyle, HCItem, HCTextItem,
  HCDrawItem, HCCustomData, HCRichData, HCViewData, HCSectionData, EmrElementItem,
  HCCommon, HCRectItem, EmrGroupItem, Generics.Collections, Winapi.Messages;

type
  TEmrGridView = class(THCGridView)
  end;

implementation

end.
