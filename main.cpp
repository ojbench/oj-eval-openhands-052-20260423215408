
#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <sstream>

using namespace std;

class term {
public:
    long long a;
    int b, c, d;

    term() : a(0), b(0), c(0), d(0) {}
    term(long long _a, int _b, int _c, int _d) : a(_a), b(_b), c(_c), d(_d) {}

    bool operator==(const term& other) const {
        return b == other.b && c == other.c && d == other.d;
    }

    bool operator<(const term& other) const {
        if (b != other.b) return b > other.b;
        if (c != other.c) return c > other.c;
        return d > other.d;
    }
};

class poly {
public:
    vector<term> terms;

    poly() {}
    poly(long long a) {
        if (a != 0) terms.push_back(term(a, 0, 0, 0));
    }
    poly(term t) {
        if (t.a != 0) terms.push_back(t);
    }

    void simplify() {
        if (terms.empty()) return;
        sort(terms.begin(), terms.end());
        vector<term> simplified;
        for (const auto& t : terms) {
            if (t.a == 0) continue;
            if (!simplified.empty() && simplified.back() == t) {
                simplified.back().a += t.a;
                if (simplified.back().a == 0) {
                    simplified.pop_back();
                }
            } else {
                simplified.push_back(t);
            }
        }
        terms = simplified;
    }

    poly operator+(const poly& other) const {
        poly res = *this;
        res.terms.insert(res.terms.end(), other.terms.begin(), other.terms.end());
        res.simplify();
        return res;
    }

    poly operator-(const poly& other) const {
        poly res = *this;
        for (auto t : other.terms) {
            t.a = -t.a;
            res.terms.push_back(t);
        }
        res.simplify();
        return res;
    }

    poly operator*(const poly& other) const {
        poly res;
        for (const auto& t1 : terms) {
            for (const auto& t2 : other.terms) {
                res.terms.push_back(term(t1.a * t2.a, t1.b + t2.b, t1.c + t2.c, t1.d + t2.d));
            }
        }
        res.simplify();
        return res;
    }

    poly derivate() const {
        poly res;
        for (const auto& t : terms) {
            // (ax^b sin^c x cos^d x)' = a [ b x^{b-1} sin^c x cos^d x + c x^b sin^{c-1} x cos^{d+1} x - d x^b sin^{c+1} x cos^{d-1} x ]
            if (t.b > 0) {
                res.terms.push_back(term(t.a * t.b, t.b - 1, t.c, t.d));
            }
            if (t.c > 0) {
                res.terms.push_back(term(t.a * t.c, t.b, t.c - 1, t.d + 1));
            }
            if (t.d > 0) {
                res.terms.push_back(term(-t.a * t.d, t.b, t.c + 1, t.d - 1));
            }
        }
        res.simplify();
        return res;
    }

    string toString() const {
        if (terms.empty()) return "0";
        string res = "";
        for (size_t i = 0; i < terms.size(); ++i) {
            const auto& t = terms[i];
            long long a = t.a;
            if (a > 0 && i > 0) res += "+";
            if (a == -1 && (t.b > 0 || t.c > 0 || t.d > 0)) {
                res += "-";
            } else if (a == 1 && (t.b > 0 || t.c > 0 || t.d > 0)) {
                // do nothing
            } else if (a != 0 || (t.b == 0 && t.c == 0 && t.d == 0)) {
                res += to_string(a);
            }

            if (t.b > 0) {
                res += "x";
                if (t.b > 1) res += "^" + to_string(t.b);
            }
            if (t.c > 0) {
                res += "sinx";
                if (t.c > 1) res += "^" + to_string(t.c);
            }
            if (t.d > 0) {
                res += "cosx";
                if (t.d > 1) res += "^" + to_string(t.d);
            }
        }
        return res;
    }
};

class frac {
public:
    poly p, q;

    frac() : q(1) {}
    frac(poly _p) : p(_p), q(1) {}
    frac(poly _p, poly _q) : p(_p), q(_q) {}

    frac operator+(const frac& other) const {
        return frac(p * other.q + q * other.p, q * other.q);
    }
    frac operator-(const frac& other) const {
        return frac(p * other.q - q * other.p, q * other.q);
    }
    frac operator*(const frac& other) const {
        return frac(p * other.p, q * other.q);
    }
    frac operator/(const frac& other) const {
        return frac(p * other.q, q * other.p);
    }

    frac derivate() const {
        return frac(p.derivate() * q - p * q.derivate(), q * q);
    }

    void output() const {
        string s1 = p.toString();
        string s2 = q.toString();
        if (s2 == "1") {
            cout << s1 << endl;
        } else if (s1 == "0") {
            cout << "0" << endl;
        } else {
            if (p.terms.size() > 1) cout << "(" << s1 << ")";
            else cout << s1;
            cout << "/";
            if (q.terms.size() > 1) cout << "(" << s2 << ")";
            else cout << s2;
            cout << endl;
        }
    }
};


class Parser {
    string s;
    int pos;

    char peek() {
        if (pos < s.length()) return s[pos];
        return 0;
    }

    char consume() {
        return s[pos++];
    }

    bool match(string prefix) {
        if (s.substr(pos, prefix.length()) == prefix) {
            pos += prefix.length();
            return true;
        }
        return false;
    }

    long long parseLong() {
        long long res = 0;
        bool neg = false;
        if (peek() == '-') {
            neg = true;
            consume();
        }
        while (isdigit(peek())) {
            res = res * 10 + (consume() - '0');
        }
        return neg ? -res : res;
    }

    int parseInt() {
        int res = 0;
        while (isdigit(peek())) {
            res = res * 10 + (consume() - '0');
        }
        return res;
    }

public:
    Parser(string _s) : s(_s), pos(0) {}

    frac parseExpression() {
        frac res = parseTerm();
        while (peek() == '+' || peek() == '-') {
            char op = consume();
            frac next = parseTerm();
            if (op == '+') res = res + next;
            else res = res - next;
        }
        return res;
    }

    frac parseTerm() {
        frac res = parseFactor();
        while (peek() == '*' || peek() == '/') {
            char op = consume();
            frac next = parseFactor();
            if (op == '*') res = res * next;
            else res = res / next;
        }
        return res;
    }

    frac parseFactor() {
        if (peek() == '(') {
            consume();
            frac res = parseExpression();
            consume(); // ')'
            return res;
        } else if (peek() == '-') {
            consume();
            return frac(poly(0)) - parseFactor();
        } else {
            return parseBaseTerm();
        }
    }

    frac parseBaseTerm() {
        long long a = 1;
        bool hasA = false;
        if (isdigit(peek())) {
            a = parseLong();
            hasA = true;
        }
        int b = 0, c = 0, d = 0;
        if (peek() == 'x') {
            consume();
            if (peek() == '^') {
                consume();
                b = parseInt();
            } else b = 1;
        }
        if (match("sinx")) {
            if (peek() == '^') {
                consume();
                c = parseInt();
            } else c = 1;
        }
        if (match("cosx")) {
            if (peek() == '^') {
                consume();
                d = parseInt();
            } else d = 1;
        }
        return frac(poly(term(a, b, c, d)));
    }
};

int main() {
    string s;
    if (!(cin >> s)) return 0;
    Parser p(s);
    frac f = p.parseExpression();
    f.output();
    f.derivate().output();
    return 0;
}

