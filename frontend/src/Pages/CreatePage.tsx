import React, { ChangeEvent, FormEvent } from "react";
import { Container } from "../Components/Container";
import { RouteComponentProps, Link } from "react-router-dom";
import { IToast, Toast } from "../Components/Toast";
import { ajax } from "rxjs/ajax";
import "./CreatePage.css";

interface Props extends RouteComponentProps {}

interface State {
  title: string;
  duration: number;
  max_members: number;
  toast?: IToast;
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
      this.setState({
        toast: {
          message: "Please input title",
          duration: 5000
        }
      });
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
    const { toast } = this.state;

    return (
      <div className="page page--create">
        <header className="page__header">
          <Container className={["flex__box", "flex__box--vc"]}>
            <h4 className="flex__item">
              <Link to="/">Home</Link>
            </h4>
          </Container>
        </header>
        <Container>
          <div style={{ position: "relative" }}>
            {toast ? (
              <Toast
                toast={toast}
                onEnd={() => {
                  this.setState({
                    toast: undefined
                  });
                }}
              />
            ) : null}
          </div>
        </Container>
        <main className="page__content">
          <Container>
            <form onSubmit={this.submit}>
              <h4 className="form__title text--center">Create Channel</h4>
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
              <div className="form__field">
                <label>Type</label>
                <select className="form__control">
                  <option value="private">Private</option>
                  <option value="public">Public</option>
                </select>
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
        </main>
      </div>
    );
  }
}
