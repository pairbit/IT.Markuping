namespace IT.Markuping.Extensions;

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