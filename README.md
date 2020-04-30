# Delphi 算法与数据结构

[TOC]



## 长串

* String类型的长串变量只是一个指向特定格式的内存块的指针。

* 换句话说，Sizeof(串变量)=sizeof(指针)。

* 如果此指针为nil，串即为空串，否则，指针将直接指向组成串的字符序列。

* 运行时库中的长串例程会确保这个序列总是以null结束。

* 字符串前面的4个字节（32位）是一个整型值，其中包含了该串的长度，再前面4个字节是另一个整型值，保存了该串的引用次数。

  ```pascal
  MyotherString := MyString;
  // 编译器会将上面的代码转换成两个步骤，1是使MyString所指向的串的引用次数递增1，2.把MyOtherString指针置为与MyString指针相同。
  ```

## 使用const

* 如果某个串传递到程序中，并且不会进行修改，那么最好把它声明为const，因为如果是变量的话会自动添加一个不可见的Try......finally模块。
* 如果不定义成常量，编译器会假定你可能会修改它，因此会建立一个局部的不可见的串变量来保存这个串。

## 提防自动转换

* 例如pos函数

  ```pascal
  PosofCh := Pos(SomeChar, MyString);
  ```

* 需要注意的是，编译器会把字符串转换为一个长串。即在堆上分配一个长串，并且置长度为1，然后把字符串拷贝进去，随后在调用pos函数，由于要用到一个不可见的串，因此会在程序最后包括不可见的Try.....finally模块以释放这个仅有一个字符的串。下面的代码会比上面的代码快

  ```pascal
  function TDPosCh(aCh: AnsiChar; const S: string): Integer;
  var
    i: Integer;
  begin
    Result := 0l;
    for i := 1 to Length(s) do
      if (S[i] = aCh) then
      begin
        Result := i;
        Exit;
      end;
  end;
  ```

* 还有，串连接符 **“+”** 也只能作用于串。



## 二、数组

* 数组的优点：
  1. 访问第N个元素是一个O(1)的操作。

```pascal
// 第N个元素的地址
AddressOfElementN := AddressOfArray + (N * sizeof(ElementType));
// 从元素X开始，元素N的地址
AddressOfElemetN := AddressOfArray + ((N - X) * sizeof(ElementType));
// 可以看到第二个计算比第一个计算稍微多一布计算工作，所以数组下标最好从0开始
```

* 数组的缺点
  1. 数组元素的插入和删除，如果数组中要插入或者删除一个元素会产生一个空位，该元素开始的所有元素必须要移动一个位置。被移动的内存量取决于n。
  2. 我们需要维护数组元素的个数
* 动态数组的缺点
  1. 如何检查数组越界。

* 创建数组类
  1. 根据需求来分配元素，甚至可以扩展或者缩小数组的大小。

### 数组类设计

```pascal
// 1. Create构造函数要保留元素大小，并对元素大小进行计算，出于速度考虑，将元素的大小设计为4字节的倍数。注意在delphi1中堆分配的空间最大为64KB, 所以要考虑不能超过该大小。
// 关于64KB的考虑：可能是因为Delphi1中Integer类型是16位的，在以后的版本中Integer类型是32为的。内存是以字节为单位存储的。所以Delphi1中堆可以分配2的16次方也就是65536个字节，而在以后的版本中可以分配2的32次方个字节。

constructor Create(aElementSize: Integer);
begin
  FActElemSize := aElementSize;
  FElementSize := (aElementSize)
end;

// 在数组类中Capacity字段标识当前数组对象的容量，Count字段标识当前数组对象中真正的存储个数，当Count = Capacity 时再插入数据需要扩展Capacity，当Capacity = MaxCount 时不允许继续添加数据

procedure rlExpand;
var
  NewCapacity: Integer;
begin
  if Capacity = 0 then
    NewCapacity := 4
  {如果当前容量小于64，则使新容量在当前基础上增加16个元素}
  else if (Capacity < 64) then
    NewCapacity := Capacity + 16
  {如果当前容量大于等于64，则使新容量在增加当前容量的1/4}
  else
    NewCapacity := Capacity + (Capacity div 4);
  {确保不至于超出数组的上限}
  if (NewCapacity > FMaxElemCount) then
    NewCapacity := FMaxElemCount;
  if (NewCapacity = Capacity) then
    rlError(tdeAtMaxCapacity, 'rlExpand', 0);
  Capacity := NewCapacity;
end;
```

## 节点管理器类

### 功能

* 构造对象
  1. 设置节点大小，通过参数传入，sizeof
  2. 计算每个页面中节点个数，如果节点个数>1，那么设置页面大小为1024字节，如果节点个数<1，则设置节点个数=1，设置页面大小=节点大小+sizeof(pointer)。这样设计是将页面的前4个字节设置为页头，从而保证操作的一致性。
* 分配节点
  1. 如果空闲列表为空，那么设置空闲列表，然后弹出空闲列表中的第一个节点，否则直接弹出第一个节点。
  2. 如何设置空闲列表，分配一个新的页面（就是获取页面大小的内存，使用PAnsiChar，这样可以直接做指针的加减），将头sizeof(Pointer)字节的内存作为头，然后循环将节点大小（FNodeSize）的内存块添加到空闲列表中。



