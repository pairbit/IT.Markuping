using IT.Markuping.Interfaces;
using System;

namespace IT.Markuping;

public class MarkupEncodingInfo
{
    private readonly int[] _codePages = null!;
    private readonly MarkupEncoding<byte> _encoding;
    private readonly ITagFinder<byte> _tagFinder;

    public MarkupEncoding<byte> Encoding => _encoding;

    public ReadOnlySpan<int> CodePages => _codePages;

    public ITagFinder<byte> TagFinder => _tagFinder;

    public MarkupEncodingInfo(MarkupEncoding<byte> encoding, int[] codePages, ITagFinder<byte> tagFinder)
    {
        if (codePages == null) throw new ArgumentNullException(nameof(codePages));
        if (codePages.Length == 0) throw new ArgumentException("empty", nameof(codePages));
        if (tagFinder == null) throw new ArgumentNullException(nameof(tagFinder));

        _encoding = encoding;
        _codePages = codePages;
        _tagFinder = tagFinder;
    }
}