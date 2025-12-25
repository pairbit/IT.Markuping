namespace IT.Markuping.Implementation;

public static class TagFinderInt32
{
    public static readonly OtherSpacesTagFinder<int> Utf32 = new((MarkupTokens<int>)MarkupAlphabets.Int32.Utf32);
    public static readonly OtherSpacesTagFinder<int> Utf32BE = new((MarkupTokens<int>)MarkupAlphabets.Int32.Utf32BE);
}