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

public static class xTagEndings
{
    //3
    private const TagEndings AnyAttributes = TagEndings.HasNoAttributes | TagEndings.HasAttributes;

    //15
    private const TagEndings Any = TagEndings.Closing | TagEndings.SelfClosing | AnyAttributes;

    public static bool IsAnyClosing(this TagEndings endings) =>
        endings == TagEndings.AnyClosing || endings == AnyAttributes;

    public static bool IsValid(this TagEndings endings) => endings <= Any;

    public static bool HasAttributes(this TagEndings endings)
        => (endings & TagEndings.HasAttributes) == TagEndings.HasAttributes;

    public static bool HasNoAttributes(this TagEndings endings)
        => (endings & TagEndings.HasNoAttributes) == TagEndings.HasNoAttributes;

    public static bool HasClosing(this TagEndings endings)
        => (endings & TagEndings.Closing) == TagEndings.Closing;

    public static bool HasSelfClosing(this TagEndings endings)
        => (endings & TagEndings.SelfClosing) == TagEndings.SelfClosing;
}