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
  
  
  
## 五、排序

* 排序的基础知识

  1. 在所有的以比较来排序的算法中，最快的速度是O(nlog(n))。
  2. 排序分为稳定排序和非稳定排序
  3. 逆序表可以测试出许多排序效率底下

* 冒泡排序

  ```pascal
  // 冒小泡
  procedure TDBubbleSort(aList    : TList;
                         aFirst   : integer;
                         aLast    : integer;
                         aCompare : TtdCompareFunc);
  var
    i, j : integer;
    Temp : pointer;
    Done : boolean;
  begin
    // 检查输入的值是否正确：例如List为空，first和last是否越界等。
    TDValidateListRange(aList, aFirst, aLast, 'TDBubbleSort');
    for i := aFirst to pred(aLast) do
    begin
      Done := true;
      for j := aLast downto succ(i) do
        if (aCompare(aList.List^[j], aList.List^[j-1]) < 0) then
        begin
          {swap jth and (j-1)th elements}
          Temp := aList.List^[j];
          aList.List^[j] := aList.List^[j-1];
          aList.List^[j-1] := Temp;
          Done := false;
        end;
      if Done then
        Exit;
    end;
  end;
  ```

  1. 冒泡排序是不稳定排序，如果把**小于**改为**小于等于**，那么就变为稳定排序，不过bDone就没有用了。
  2. 时间复杂度是O(n²)

* 摇动排序

  ```pascal
  procedure TDShakerSort(aList    : TList;
                         aFirst   : integer;
                         aLast    : integer;
                         aCompare : TtdCompareFunc);
  var
    i    : integer;
    Temp : pointer;
  begin
    TDValidateListRange(aList, aFirst, aLast, 'TDShakerSort');
    while (aFirst < aLast) do begin
      for i := aLast downto succ(aFirst) do
        if (aCompare(aList.List^[i], aList.List^[i-1]) < 0) then
        begin
          Temp := aList.List^[i];
          aList.List^[i] := aList.List^[i-1];
          aList.List^[i-1] := Temp;
        end;
      inc(aFirst);
      for i := succ(aFirst) to aLast do
        if (aCompare(aList.List^[i], aList.List^[i-1]) < 0) then
        begin
          Temp := aList.List^[i];
          aList.List^[i] := aList.List^[i-1];
          aList.List^[i-1] := Temp;
        end;
      dec(aLast);
    end;
  end;
  ```

  1. 摇动排序是冒泡排序的一种变种，这种算法比冒泡算法的速度稍微有提高。
  2. 摇动排序是不稳定排序

* 选择排序

  ```pascal
  procedure TDSelectionSort(aList    : TList;
                            aFirst   : integer;
                            aLast    : integer;
                            aCompare : TtdCompareFunc);
  var
    i, j       : integer;
    IndexOfMin : integer;
    Temp       : pointer;
  begin
    TDValidateListRange(aList, aFirst, aLast, 'TDSelectionSort');
    for i := aFirst to pred(aLast) do
    begin
      IndexOfMin := i;
      for j := succ(i) to aLast do
        if (aCompare(aList.List^[j], aList.List^[IndexOfMin]) < 0) then
          IndexOfMin := j;
      Temp := aList.List^[i];
      aList.List^[i] := aList.List^[IndexOfMin];
      aList.List^[IndexOfMin] := Temp;
    end;
  end;
  ```

  1. 选择排序的第一次要比较n此，第二次比较（n-1）次，总次数等于n(n+1)/2 - 1，即O(n²)。
  2. 在实际工作中，比较一次的开销比交换两个元素的开销要小的多，所以选择排序速度还可以。
  3. 选择排序是稳定排序。

* 插入排序

  ```pascal
  // 未优化的插入排序
  procedure TDInsertionSortStd(aList    : TList;
                               aFirst   : integer;
                               aLast    : integer;
                               aCompare : TtdCompareFunc);
  var
    i, j : integer;
    Temp : pointer;
  begin
    TDValidateListRange(aList, aFirst, aLast, 'TDInsertionSortStd');
    for i := succ(aFirst) to aLast do
    begin
      Temp := aList.List^[i];
      j := i;
      while (j > aFirst) and (aCompare(Temp, aList.List^[j-1]) < 0) do
      begin
        aList.List^[j] := aList.List^[j-1];
        dec(j);
      end;
      aList.List^[j] := Temp;
    end;
  end;
  
  // 已优化的插入排序
  //1. 先使用选择排序找到最小的元素
  //2. 之后使用插入排序排后面的元素
  procedure TDInsertionSort(aList    : TList;
                            aFirst   : integer;
                            aLast    : integer;
                            aCompare : TtdCompareFunc);
  var
    i, j       : integer;
    IndexOfMin : integer;
    Temp       : pointer;
  begin
    TDValidateListRange(aList, aFirst, aLast, 'TDInsertionSort');
    //{find the smallest element and put it in the first position}
    IndexOfMin := aFirst;
    for i := succ(aFirst) to aLast do
      if (aCompare(aList.List^[i], aList.List^[IndexOfMin]) < 0) then
        IndexOfMin := i;
    if (aFirst <> IndexOfMin) then
    begin
      Temp := aList.List^[aFirst];
      aList.List^[aFirst] := aList.List^[IndexOfMin];
      aList.List^[IndexOfMin] := Temp;
    end;
    //{now sort via insertion method}
    for i := aFirst+2 to aLast do
    begin
      Temp := aList.List^[i];
      j := i;
      while (aCompare(Temp, aList.List^[j-1]) < 0) do
      begin
        aList.List^[j] := aList.List^[j-1];
        dec(j);
      end;
      aList.List^[j] := Temp;
    end;
  end;
  ```

  1. 插入排序也是一种O(n²)的排序。
  2. 如果列表部分有序，那么插入排序是一种非常快的算法。甚至可以是一种O(n)的算法。
  3. 插入排序是一种稳定的排序算法。

* 希尔排序（Shell Sort）

  ```pascal
  procedure TDShellSort(aList    : TList;
                        aFirst   : integer;
                        aLast    : integer;
                        aCompare : TtdCompareFunc);
  var
    i, j : integer;
    h    : integer;
    Temp : pointer;
    Ninth: integer;
  begin
    //{Note: Shellsort was invented by Donald Shell in 1959.
    //       This Shellsort implementation uses Knuth's sequence: 1, 4,
    //       13, 40, 121, ...}
    TDValidateListRange(aList, aFirst, aLast, 'TDShellSort');
    //{firstly calculate the first h value we shall use: it'll be about
    // one ninth of the number of the elements}
    h := 1;
    Ninth := (aLast - aFirst) div 9;
    while (h <= Ninth) do
      h := (h * 3) + 1;
    //{start a loop that'll decrement h by one third each time through}
    while (h > 0) do begin
      {now insertion sort each h-subfile}
      for i := (aFirst + h) to aLast do
      begin
        Temp := aList.List^[i];
        j := i;
        while (j >= (aFirst+h)) and
              (aCompare(Temp, aList.List^[j-h]) < 0) do
        begin
          aList.List^[j] := aList.List^[j-h];
          dec(j, h);
        end;
        aList.List^[j] := Temp;
      end;
      //{decrease h by a third}
      h := h div 3;
    end;
  end;
  ```

  1. 希尔排序是基于插入排序的一种排序

* 梳式排序（Comb Sort）

  ```pascal
  procedure TDCombSort(aList    : TList;
                       aFirst   : integer;
                       aLast    : integer;
                       aCompare : TtdCompareFunc);
  var
    i, j : integer;
    Temp : pointer;
    Done : boolean;
    Gap  : integer;
  begin
    TDValidateListRange(aList, aFirst, aLast, 'TDCombSort');
    //{start off with a gap equal to the number of elements}
    Gap := succ(aLast - aFirst);
    repeat
      //{assume we'll finish this time around}
      Done := true;
      //{calculate the new gap}
      Gap := (longint(Gap) * 10) div 13; {Gap := Trunc(Gap / 1.3);}
      if (Gap < 1) then
        Gap := 1
      else if (Gap = 9) or (Gap = 10) then
        Gap := 11;
      {order every item with its sibling Gap items along}
      for i := aFirst to (aLast - Gap) do begin
        j := i + Gap;
        if (aCompare(aList.List^[j], aList.List^[i]) < 0) then
        begin
          //{swap jth and (j-Gap)th elements}
          Temp := aList.List^[j];
          aList.List^[j] := aList.List^[i];
          aList.List^[i] := Temp;
          //{we swapped, so we didn't finish}
          Done := false;
        end;
      end;
    until Done and (Gap = 1);
  end;
  ```

  

  



## 八、 树

* 单链表如果竖起来，就可以看成是一个特殊的树，每个节点只有一个孩子的树，被称为**单叉树**。
* **多叉树**是对上面概念的推广。这是节点的一个集合，把节点组织起来，以使除了根（树顶上的节点被定义为根，将没有孩子的节点成为叶子）以外的所有几点只有一个父亲。如果每个节点最多有n个孩子，则被成为**N**叉树
* 为了保持统一性，使用一个虚拟根节点，真正的根节点作为虚拟根节点的左孩子。
* 堆就是完全二叉树

![二叉树](.\pic\二叉树.png)

* 如果要使用二叉树，那么就要能够**添加**，**删除**，**遍历节点**，
* 先序：d, b, a, c, f, e, g
* 中序： a, b, c, d, e, f, g
* 后序：a, c, b, e, g, f, d
* 下面是函数原型

```pascal
// 添加节点
// 1. 当前节点，2. 左孩子还是有孩子， 3. 要插入的节点
function InsertAt(aParentNode: PtdBinTreeNode; aChildType: TtdChildType; aItem: pointer) : PtdBinTreeNode;
// 删除节点
// 删除规则：1. 如果该节点有两个孩子，那么无法删除该节点，2. 如果只有一个孩子，则删除该节点并将孩子节点替换自己，3. 如果没有孩子则直接删除
procedure Delete(aNode: PtdBinTreeNode);
// 遍历节点
// 有先序遍历，中序遍历，后序遍历和层次遍历，层次遍历最好理解，但是编码最麻烦。

```

### 树的发展，从二叉查找树讲起

1. **二叉查找树**是基于二叉树的中序遍历而来的，它规定了树的左孩子小于自己（这个使用关键字实现，如果两个节点的值相同，那么我们可以定义一个关键字，这个关键字不会重复，然后利用关键字来排序使得所有的节点的关键字都不相同），右孩子大于自己。这种情况下正好是二叉树**中序遍历**的顺序（从大到小）。

2. 由于上面的二叉查找树会造成二叉树的**退化**（二叉树会退化成一个链表），那么在查找一个节点的时候会造成有的节点查找很快，可能是O(1)，而有的节点可能要O(n)。随着二叉树的发展，有了**左旋**操作和**右旋**操作，通过节点的左旋和右旋可以使节点**升阶或者降阶**。

3. 有了树的左旋和右旋，发展出了**伸展树**，伸展树认为**当前搜索**的节点在**下一次搜索的时候**还可能会**被搜索**，所以当前搜索的节点会被升级到根节点或者根节点下面的节点，也就是**第二级节点**，伸展树从一定程度上解决了树在退化的情况下**搜索速度变慢的问题**，但是没有从根本**上解决树不平衡的问题**。

4. 为了解决树的平衡问题，有了红黑树（**在红黑树中不允许存在节点重复**），红黑树有着3点规则

   （1）树外层节点中的空孩子链接被假设为指向其他节点（当然是不存在的）。这些不可见的空接点被成为外部节点。

   （2）黑条件：从根到每个外部节点的路径都包括相同数目的黑节点

   （3）红条件：不是根的红节点有一个黑父亲。

![简单的红黑树](.\pic\简单的红黑树.png)

