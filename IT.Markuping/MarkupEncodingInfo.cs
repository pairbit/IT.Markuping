using System;

namespace IT.Markuping;

public class MarkupEncodingInfo
{
    private readonly int[] _codePages = null!;
    private readonly MarkupEncoding<byte> _encoding;

    public MarkupEncoding<byte> Encoding => _encoding;

    public ReadOnlySpan<int> CodePages => _codePages;

    public MarkupEncodingInfo(MarkupEncoding<byte> encoding, int[] codePages)
    {
        if (codePages == null) throw new ArgumentNullException(nameof(codePages));
        if (codePages.Length == 0) throw new ArgumentException("empty", nameof(codePages));

        _encoding = encoding;
        _codePages = codePages;
    }
}