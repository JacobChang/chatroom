import React, { ChangeEvent, FormEvent } from "react";
import { Container } from "../Components/Container";
import { RouteComponentProps, Link } from "react-router-dom";
import { ajax } from "rxjs/ajax";
import "./CreatePage.css";

interface Props extends RouteComponentProps {}

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
    if (this.state.title === "") {
      return;
    }

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
        this.props.history.replace("/");
      });
  };

  render() {
    return (
      <div className="page page--create">
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
                <label>Duration(1min - 60mins)</label>
                <input
                  className="form__control"
                  type="number"
                  min="1"
                  max="60"
                  placeholder="Duration(mins) of channel"
                  value={this.state.duration}
                  onChange={this.updateDuration}
                />
              </div>
              <div className="form__field">
                <label>Number of members(1 - 100)</label>
                <input
                  className="form__control"
                  type="number"
                  min="1"
                  max="100"
                  placeholder="Members of channel"
                  value={this.state.max_members}
                  onChange={this.updateMaxMembers}
                />
              </div>
              <div className="form__submit">
                <div className="button__group">
                  <Link
                    className="button button--outline button--primary"
                    to="/"
                  >
                    Cancel
                  </Link>
                  <button
                    className="button button--solid button--primary"
                    type="submit"
                  >
                    Create
                  </button>
                </div>
              </div>
            </form>
          </Container>
        </section>
      </div>
    );
  }
}
