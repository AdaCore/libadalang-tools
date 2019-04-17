def show_nonprintable(text):
    """
    Transform `text` as if it was pipe'd through `cat -v`.

    :type text: str
    :rtype: str
    """
    def escape(char):
        ochar = ord(char)
        if ochar > 0x7f:
            return 'M-' + escape(chr(ochar - 0o200))

        if char in ('\n', '\t'):
            return char
        elif ochar < ord(' '):
            return '^' + chr(ochar + ord('@'))
        elif ochar == 0x7f:
            return '^?'
        else:
            return char

    return ''.join(escape(c) for c in text)
