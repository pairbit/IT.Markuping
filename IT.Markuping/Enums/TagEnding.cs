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