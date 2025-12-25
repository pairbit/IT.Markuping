using System;

namespace IT.Markuping;

public class MarkupEncodingInfo<T> where T : unmanaged
{
    private readonly int[] _codePages = null!;
    private readonly MarkupEncoding<T> _encoding;

    public MarkupEncoding<T> Encoding => _encoding;

    public ReadOnlySpan<int> CodePages => _codePages;

    public MarkupEncodingInfo(MarkupEncoding<T> encoding, int[] codePages)
    {
        if (codePages == null) throw new ArgumentNullException(nameof(codePages));
        if (codePages.Length == 0) throw new ArgumentException("empty", nameof(codePages));

        _encoding = encoding;
        _codePages = codePages;
    }
}