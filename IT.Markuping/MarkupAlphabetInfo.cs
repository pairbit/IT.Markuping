using System;

namespace IT.Markuping;

public class MarkupAlphabetInfo<T> where T : unmanaged
{
    private readonly int[] _codePages = null!;
    private readonly MarkupAlphabet<T> _alphabet;

    public MarkupAlphabet<T> Alphabet => _alphabet;

    public ReadOnlySpan<int> CodePages => _codePages;

    public MarkupAlphabetInfo(MarkupAlphabet<T> alphabet, int[] codePages)
    {
        if (codePages == null) throw new ArgumentNullException(nameof(codePages));
        if (codePages.Length == 0) throw new ArgumentException("empty", nameof(codePages));

        _alphabet = alphabet;
        _codePages = codePages;
    }
}