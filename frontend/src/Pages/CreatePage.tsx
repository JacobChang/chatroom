import React, { ChangeEvent, FormEvent } from "react";
import { Container } from "../Components/Container";
import { Link } from "react-router-dom";
import { ajax } from "rxjs/ajax";

interface Props {}

interface State {
  title: string;
  duration: number;
  max_members: number;
}

export class CreatePage extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props);

    this.state = {
      title: "",
      duration: 10,
      max_members: 10
    };
  }

  updateTitle = (event: ChangeEvent<HTMLInputElement>) => {
    this.setState({
      title: event.target.value
    });
  };

  updateDuration = (event: ChangeEvent<HTMLInputElement>) => {
    this.setState({
      duration: parseInt(event.target.value, 10)
    });
  };

  updateMaxMembers = (event: ChangeEvent<HTMLInputElement>) => {
    this.setState({
      max_members: parseInt(event.target.value, 10)
    });
  };

  submit = (event: FormEvent) => {
    event.preventDefault();

    let params = {
      title: this.state.title,
      duration: this.state.duration,
      max_members: this.state.max_members
    };
    ajax
      .post("/api/v1/channels", JSON.stringify(params), {
        "Content-Type": "application/json"
      })
      .subscribe(result => {
        console.log(result);
      });
  };

  render() {
    return (
      <div className="page page--create">
        <header>
          <Container className={["flex__box", "flex__box--vc"]}>
            <h4 className="flex__item">
              <Link to="/">Back to Home</Link>
            </h4>
          </Container>
        </header>
        <section>
          <Container>
            <form onSubmit={this.submit}>
              <div className="form__field">
                <label>Title</label>
                <input
                  className="form__control"
                  type="text"
                  placeholder="Title of channel"
                  value={this.state.title}
                  onChange={this.updateTitle}
                />
              </div>
              <div className="form__field">
                <label>Duration(mins)</label>
                <input
                  className="form__control"
                  type="number"
                  placeholder="Duration(mins) of channel"
                  value={this.state.duration}
                  onChange={this.updateDuration}
                />
              </div>
              <div className="form__field">
                <label>Number of members</label>
                <input
                  className="form__control"
                  type="number"
                  placeholder="Members of channel"
                  value={this.state.max_members}
                  onChange={this.updateMaxMembers}
                />
              </div>
              <div className="form__submit">
                <button type="submit">Create Channel</button>
              </div>
            </form>
          </Container>
        </section>
      </div>
    );
  }
}
