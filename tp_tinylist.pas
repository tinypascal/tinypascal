unit tp_tinylist;

{$mode objfpc}

interface

uses Classes;

type PTinyListElement = ^TTinyListElement;
     TTinyListElement = record
       item: Pointer;
       prev, next: PTinyListElement;
     end;

     TTinyList = class(TObject)
       private
         head, tail, current: PTinyListElement;
         FCount: LongInt;

         function GetElement(index: Longint): PTinyListElement;

       public
         property Count: LongInt read FCount;

         Constructor Create; virtual;
         procedure Add(item: Pointer); virtual;
         procedure AddFirst(item: Pointer); virtual;
         procedure Insert(index: Longint; item: Pointer);
         function Get: Pointer;
         function GetFirst: Pointer;
         function Items(index: Longint): Pointer;
         function Remove: Pointer; virtual;
         function RemoveFirst: Pointer; virtual;
         function Delete(index: Longint): Pointer; virtual;
         Destructor Destroy; override;
     end;

implementation

uses math;

// get a certain item, 0 based index, expensive function!
function TTinyList.GetElement(index: Longint): PTinyListElement;
var i: LongInt;
begin
  if index <= Count div 2 then
  begin
    current := head;
    for i := 0 to index - 1  do current := current^.next;
  end
  else
  begin
    current := tail;
    for i := 0 to Count - index - 2 do current := current^.prev;
  end;

  Result := current;
end;

Constructor TTinyList.Create;
begin
  inherited;

  head := nil;
  tail := nil;
  FCount := 0;
end;

// add item to tail
procedure TTinyList.Add(item: Pointer);
begin
  new(current);
  current^.item := item;
  current^.prev := tail;
  current^.next := nil;

  if tail <> nil then tail^.next := current;
  if head = nil then head := current;

  tail := current;
  inc(FCount);
end;

// add item to head
procedure TTinyList.AddFirst(item: Pointer);
begin
  new(current);
  current^.item := item;
  current^.prev := nil;
  current^.next := head;

  if head <> nil then head^.prev := current;
  if tail = nil then tail := current;

  head := current;
  inc(FCount);
end;

// replaces the item at Position with the new one, shifts the rest up
procedure TTinyList.Insert(index: Longint; item: Pointer);
var temp: PTinyListElement;
begin
  // range check
  index := min(Count - 1, max(0, index));

  if index = 0 then AddFirst(item)
  else
  if index = Count - 1 then Add(item)
  else
  begin
    temp := GetElement(index);

    new(current);
    current^.item := item;

    current^.prev := temp^.prev;
    current^.prev^.next := current;

    current^.next := temp;
    temp^.prev := current;

    inc(FCount);
  end;
end;

// get item from tail
function TTinyList.Get: Pointer;
begin
  if tail <> nil then Result := tail^.item
                 else Result := nil;
end;

// get item from head
function TTinyList.GetFirst: Pointer;
begin
  if head <> nil then Result := head^.item
                 else Result := nil;
end;

// get a certain item, 0 based index, expensive function!
function TTinyList.Items(index: Longint): Pointer;
begin
  if (index < 0) or (index > Count - 1) then
  begin
    Result := nil;
    Exit;
  end;

  Result := GetElement(index)^.item;
end;

// remove item from tail
function TTinyList.Remove: Pointer;
begin
  if tail = nil then Exit;

  current := tail;
  tail := current^.prev;

  if tail <> nil then tail^.next := nil
                 else head := nil;

  Result := current^.item;
  Dispose(current);
  dec(FCount);
end;

// remove item from head
function TTinyList.RemoveFirst: Pointer;
begin
  if head = nil then Exit;

  current := head;
  head := current^.next;

  if head <> nil then head^.prev := nil
                 else tail := nil;

  Result := current^.item;
  Dispose(current);
  dec(FCount);
end;

// deletes the item at index position
function TTinyList.Delete(index: Longint): Pointer;
begin
  if (index < 0) or (index > Count - 1) then
  begin
    Result := nil;
    Exit;
  end;

  if index = 0 then RemoveFirst
  else
  if index = Count - 1 then Remove
  else
  begin
    current := GetElement(index);
    current^.next^.prev := current^.prev;
    current^.prev^.next := current^.next;

    Result := current^.item;
    Dispose(current);

    dec(FCount);
  end;
end;

// empty list
Destructor TTinyList.Destroy;
begin
  while Count > 0 do RemoveFirst;

  inherited;
end;

end.

