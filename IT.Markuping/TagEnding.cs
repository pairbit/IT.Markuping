namespace IT.Markuping;

public enum TagEnding : byte
{
    None = 0,

    /// <summary>
    /// &lt;Tag
    /// </summary>
    Name = 1,

    /// <summary>
    /// &lt;Tag&gt;
    /// </summary>
    Closing = 2,

    /// <summary>
    /// &lt;Tag/&gt;
    /// </summary>
    SelfClosing = 3,

    /// <summary>
    /// &lt;Tag\n \r
    /// </summary>
    AttributeStart = 4,

    /// <summary>
    /// &lt;Tag a&gt;
    /// </summary>
    ClosingHasAttributes = 5,

    /// <summary>
    /// &lt;Tag a/&gt;
    /// </summary>
    SelfClosingHasAttributes = 6,
}

public static class xTagEnding
{
    public static bool IsClosing(this TagEnding ending) =>
        ending == TagEnding.Closing ||
        ending == TagEnding.ClosingHasAttributes;

    public static bool IsSelfClosing(this TagEnding ending) =>
        ending == TagEnding.SelfClosing ||
        ending == TagEnding.SelfClosingHasAttributes;

    public static bool IsAnyClosingHasAttributes(this TagEnding ending) =>
        ending == TagEnding.ClosingHasAttributes ||
        ending == TagEnding.SelfClosingHasAttributes;

    public static bool IsAnyClosingNoAttributes(this TagEnding ending) =>
        ending == TagEnding.Closing ||
        ending == TagEnding.SelfClosing;

    public static bool IsAnyClosing(this TagEnding ending) =>
        ending.IsClosing() || ending.IsSelfClosing();

    public static bool IsNoClosing(this TagEnding ending) =>
        ending == TagEnding.Name ||
        ending == TagEnding.AttributeStart;
}