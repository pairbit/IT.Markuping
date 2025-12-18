using System;

namespace IT.Markuping;

[Flags]
public enum TagEndings : byte
{
    /// <summary>
    /// &lt;Tag&gt; or &lt;Tag \n a&gt; or &lt;Tag/&gt; or &lt;Tag \n a/&gt;
    /// </summary>
    AnyClosing = 0,

    /// <summary>
    /// &lt;Tag&gt; or &lt;Tag/&gt; or "&lt;Tag " or "&lt;Tag\n"
    /// </summary>
    /// <remarks>
    /// указание данного параметра без Closing или SelfClosing 
    /// позволяет искать конец названия тега "&lt;Tag "
    /// </remarks>
    HasNoAttributes = 1,

    /// <summary>
    /// "&lt;Tag " or "&lt;Tag \n "
    /// </summary>
    /// <remarks>
    /// указание данного параметра без Closing или SelfClosing 
    /// позволяет искать начало атрибута 
    /// </remarks>
    HasAttributes = 2,

    /// <summary>
    /// &lt;Tag&gt; or &lt;Tag \n a&gt;
    /// </summary>
    Closing = 4,

    /// <summary>
    /// &lt;Tag&gt; or &lt;Tag \n &gt;
    /// </summary>
    ClosingHasNoAttributes = Closing | HasNoAttributes,

    /// <summary>
    /// &lt;Tag \n a&gt;
    /// </summary>
    ClosingHasAttributes = Closing | HasAttributes,

    /// <summary>
    /// &lt;Tag/&gt; or &lt;Tag \n a/&gt;
    /// </summary>
    SelfClosing = 8,

    /// <summary>
    /// &lt;Tag/&gt; or &lt;Tag \n /&gt;
    /// </summary>
    SelfClosingHasNoAttributes = SelfClosing | HasNoAttributes,

    /// <summary>
    /// &lt;Tag a/&gt; or &lt;Tag \n a/&gt;
    /// </summary>
    SelfClosingHasAttributes = SelfClosing | HasAttributes,

    /// <summary>
    /// &lt;Tag&gt; or &lt;Tag/&gt; or &lt;Tag \n &gt; or &lt;Tag \n /&gt;
    /// </summary>
    AnyClosingHasNoAttributes = Closing | SelfClosing | HasNoAttributes,

    /// <summary>
    /// &lt;Tag a&gt; or &lt;Tag a/&gt; or &lt;Tag \n a&gt; or &lt;Tag \n a/&gt;
    /// </summary>
    AnyClosingHasAttributes = Closing | SelfClosing | HasAttributes
}