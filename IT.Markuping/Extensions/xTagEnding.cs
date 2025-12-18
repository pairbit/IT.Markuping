namespace IT.Markuping.Extensions;

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

    public static bool IsAnyClosingHasNoAttributes(this TagEnding ending) =>
        ending == TagEnding.Closing ||
        ending == TagEnding.SelfClosing;

    public static bool IsAnyClosing(this TagEnding ending) =>
        ending.IsClosing() || ending.IsSelfClosing();

    public static bool IsNoClosing(this TagEnding ending) =>
        ending == TagEnding.Name ||
        ending == TagEnding.AttributeStart;
}