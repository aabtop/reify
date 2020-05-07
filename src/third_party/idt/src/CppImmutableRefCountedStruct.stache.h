// clang-format off
class {{name}} {
 public:
  struct Data {
{{#members}}
    {{{type}}} {{name}};
{{/members}}
  };

  {{name}}() = delete;
  {{name}}(const {{name}}&) = delete;
  {{name}}({{name}}&&) = delete;

  {{name}}(const Data& data) : data_(data) {}
  static std::shared_ptr<{{name}}> make_shared(const Data& data) {
    return std::make_shared<{{name}}>(data);
  }

  const Data& data() const { return data_; }

 private:
  const Data data_;
};
